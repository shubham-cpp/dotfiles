import { spawn, type ChildProcess } from "node:child_process";
import { existsSync } from "node:fs";
import path from "node:path";
import process from "node:process";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "idle-inhibit";
const WATCH_NAME = "pi-idle-inhibit-watch";
const WHO = "pi-idle-inhibit";
const WHY = "Pi agent running";
const STOP_KILL_MS = 1000;

type InhibitorSpec = {
	command: string;
	args: string[];
};

type State = {
	holds: number;
	generation: number;
	child: ChildProcess | undefined;
	spec: InhibitorSpec | undefined;
	warnedUnavailable: boolean;
	warnedSpawnFail: boolean;
	childRetryUsed: boolean;
	queue: Promise<void>;
};

const state: State = {
	holds: 0,
	generation: 0,
	child: undefined,
	spec: undefined,
	warnedUnavailable: false,
	warnedSpawnFail: false,
	childRetryUsed: false,
	queue: Promise.resolve(),
};

function commandExists(command: string): boolean {
	const searchPath = process.env.PATH ?? "";
	for (const directory of searchPath.split(":")) {
		if (!directory) continue;
		if (existsSync(path.join(directory, command))) return true;
	}
	return false;
}

function getInhibitorSpec(): InhibitorSpec | undefined {
	if (process.platform === "darwin") {
		if (!commandExists("caffeinate")) return undefined;
		return { command: "caffeinate", args: ["-dimsu"] };
	}
	if (process.platform === "linux") {
		if (!commandExists("systemd-inhibit")) return undefined;
		return {
			command: "systemd-inhibit",
			args: [
				"--what=idle:sleep",
				`--who=${WHO}`,
				`--why=${WHY}`,
				"--mode=block",
				"sleep",
				"infinity",
			],
		};
	}
	return undefined;
}

function parentBoundScript(): string {
	return [
		"parent=$1; shift",
		'"$@" & child=$!',
		'( while kill -0 "$parent" 2>/dev/null; do sleep 5; done; kill "$child" 2>/dev/null ) & watcher=$!',
		'cleanup() { kill "$watcher" 2>/dev/null; kill "$child" 2>/dev/null; wait "$child" 2>/dev/null; }',
		"trap 'cleanup; exit 0' INT TERM HUP EXIT",
		'wait "$child"; status=$?',
		'kill "$watcher" 2>/dev/null',
		"trap - EXIT",
		'exit "$status"',
	].join("; ");
}

function enqueue(work: () => Promise<void>): Promise<void> {
	state.queue = state.queue.then(work, work);
	return state.queue;
}

function clearStatus(ctx: ExtensionContext): void {
	if (ctx.hasUI) ctx.ui.setStatus(STATUS_KEY, undefined);
}

function setAwake(ctx: ExtensionContext): void {
	if (ctx.hasUI) ctx.ui.setStatus(STATUS_KEY, "awake");
}

function warnOnce(
	ctx: ExtensionContext,
	flag: "warnedUnavailable" | "warnedSpawnFail",
	message: string,
): void {
	if (state[flag]) return;
	state[flag] = true;
	if (ctx.hasUI) ctx.ui.notify(message, "warning");
}

async function stopChild(child: ChildProcess | undefined): Promise<void> {
	if (!child || child.exitCode !== null) return;
	await new Promise<void>((resolve) => {
		const timer = setTimeout(() => {
			if (child.exitCode === null) child.kill("SIGKILL");
		}, STOP_KILL_MS);
		child.once("exit", () => {
			clearTimeout(timer);
			resolve();
		});
		child.kill("SIGTERM");
	});
}

function attachChildExitHandler(child: ChildProcess, generation: number, ctx: ExtensionContext): void {
	child.once("exit", () => {
		void enqueue(async () => {
			if (state.generation !== generation) return;
			if (state.child !== child) return;
			state.child = undefined;
			if (state.holds <= 0) {
				clearStatus(ctx);
				return;
			}
			if (state.childRetryUsed) {
				warnOnce(ctx, "warnedSpawnFail", "idle-inhibit: inhibitor child exited while holding");
				clearStatus(ctx);
				return;
			}
			state.childRetryUsed = true;
			await startIfNeeded(ctx, generation);
		});
	});
}

function spawnWatcher(spec: InhibitorSpec): ChildProcess {
	return spawn(
		"sh",
		["-c", parentBoundScript(), WATCH_NAME, String(process.pid), spec.command, ...spec.args],
		{ stdio: "ignore" },
	);
}

async function startIfNeeded(ctx: ExtensionContext, generation: number): Promise<void> {
	if (state.generation !== generation) return;
	if (state.holds <= 0) return;
	if (state.child && state.child.exitCode === null) {
		setAwake(ctx);
		return;
	}

	const spec = state.spec ?? getInhibitorSpec();
	state.spec = spec;
	if (!spec) {
		warnOnce(
			ctx,
			"warnedUnavailable",
			"idle-inhibit: no inhibitor on this platform (need systemd-inhibit on Linux or caffeinate on macOS)",
		);
		clearStatus(ctx);
		return;
	}

	const child = spawnWatcher(spec);
	state.child = child;
	attachChildExitHandler(child, generation, ctx);

	const spawnFailed = await new Promise<boolean>((resolve) => {
		const onError = (err: Error) => {
			child.off("spawn", onSpawn);
			warnOnce(ctx, "warnedSpawnFail", `idle-inhibit: spawn failed: ${err.message}`);
			resolve(true);
		};
		const onSpawn = () => {
			child.off("error", onError);
			resolve(false);
		};
		child.once("error", onError);
		child.once("spawn", onSpawn);
	});

	if (state.generation !== generation || state.holds <= 0 || spawnFailed) {
		if (state.child === child) state.child = undefined;
		await stopChild(child);
		if (state.holds <= 0 || state.generation !== generation) clearStatus(ctx);
		return;
	}

	setAwake(ctx);
}

async function stopIfIdle(ctx: ExtensionContext, generation: number): Promise<void> {
	if (state.holds > 0 && state.generation === generation) return;
	const child = state.child;
	state.child = undefined;
	await stopChild(child);
	if (state.generation === generation) clearStatus(ctx);
}

export default function (pi: ExtensionAPI) {
	pi.on("session_shutdown", async (_event, ctx) => {
		state.generation += 1;
		state.holds = 0;
		state.childRetryUsed = false;
		await enqueue(async () => {
			await stopIfIdle(ctx, state.generation);
		});
	});

	pi.on("agent_start", async (_event, ctx) => {
		const generation = state.generation;
		state.holds += 1;
		if (state.holds === 1) state.childRetryUsed = false;
		await enqueue(async () => {
			await startIfNeeded(ctx, generation);
		});
	});

	pi.on("agent_settled", async (_event, ctx) => {
		const generation = state.generation;
		state.holds = Math.max(0, state.holds - 1);
		await enqueue(async () => {
			await stopIfIdle(ctx, generation);
		});
	});
}
