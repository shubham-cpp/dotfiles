// Compact one-line footer for pi.
//
// Quota is subscription-only: Codex (5h + weekly) and xAI SuperGrok (weekly,
// plus 5h when the billing payload actually has a short window). API-key
// auth hides the quota segment. Snapshots are cached per provider for 240s so
// switching models does not refetch, and switching providers reuses the other
// provider's last snapshot until it goes stale.
//
// After each agent run, a TUI-only custom entry shows that run's token input,
// cached input, output token, and TPS. appendEntry is not sent to the model.

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { basename } from "node:path";

type Theme = {
	fg: (token: string, text: string) => string;
	bold: (text: string) => string;
};

type FooterData = {
	getGitBranch: () => string | null;
	getExtensionStatuses: () => ReadonlyMap<string, string>;
	onBranchChange: (cb: () => void) => () => void;
};

type TuiHandle = {
	requestRender: () => void;
};

const ANSI = /\x1b\[[0-9;]*m/g;

function visibleWidth(text: string): number {
	return text.replace(ANSI, "").length;
}

function truncateToWidth(text: string, width: number, ellipsis = "…"): string {
	if (visibleWidth(text) <= width) return text;
	if (width <= 0) return "";
	const budget = Math.max(0, width - visibleWidth(ellipsis));
	let out = "";
	let used = 0;
	for (const part of text.split(/(\x1b\[[0-9;]*m)/)) {
		if (part.startsWith("\x1b")) {
			out += part;
			continue;
		}
		for (const ch of part) {
			if (used >= budget) return out + ellipsis;
			out += ch;
			used++;
		}
	}
	return out + ellipsis;
}

const GIT_BRANCH_ICON = "\uf418"; // nf-oct-git-branch
const CACHE_ICON = "\u{f001a}"; // nf-md-cached
const SEPARATOR = "\u{e0b1}"; // nf-pl-right_soft_divider
const GAUGE_WIDTH = 8;
const QUOTA_TTL_MS = 240_000;
const QUOTA_TIMEOUT_MS = 8_000;
const TURN_STATS_TYPE = "turn-stats";
const CODEX_USAGE_URL = "https://chatgpt.com/backend-api/wham/usage";
const GROK_USER_URL = "https://cli-chat-proxy.grok.com/v1/user";
const GROK_CREDITS_URL = "https://cli-chat-proxy.grok.com/v1/billing?format=credits";
const GROK_CLIENT_VERSION = "0.1.0";

type QuotaKind = "codex" | "xai";

type QuotaWindow = {
	usedPercent: number;
};

type QuotaSnapshot = {
	kind: QuotaKind;
	fiveHour?: QuotaWindow;
	weekly?: QuotaWindow;
	fetchedAt: number;
};

type SessionStats = {
	cost: number;
	cacheHit: number | undefined;
};

type TurnStats = {
	input: number;
	cacheRead: number;
	cacheWrite: number;
	output: number;
	tps: number | undefined;
	elapsedMs: number;
};

function formatTok(n: number): string {
	if (n < 1000) return String(Math.round(n));
	if (n < 10_000) return `${(n / 1000).toFixed(1)}k`;
	if (n < 1_000_000) return `${Math.round(n / 1000)}k`;
	return `${(n / 1_000_000).toFixed(1)}M`;
}

function formatTps(n: number): string {
	if (!Number.isFinite(n) || n < 0) return "";
	return n < 100 ? n.toFixed(1) : String(Math.round(n));
}

function formatTurnStats(data: TurnStats, theme: Theme): string {
	const sep = theme.fg("dim", ` ${SEPARATOR} `);
	const parts = [
		theme.fg("dim", `token input: ${formatTok(data.input)}`),
		theme.fg("dim", `cached input: ${formatTok(data.cacheRead)}`),
		theme.fg("dim", `output token: ${formatTok(data.output)}`),
	];
	if (data.tps != null) parts.push(theme.fg("muted", `TPS: ${formatTps(data.tps)}`));
	return parts.join(sep);
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null && !Array.isArray(value);
}

function asNumber(value: unknown): number | undefined {
	if (typeof value === "number" && Number.isFinite(value)) return value;
	if (typeof value === "string" && value.trim() !== "") {
		const n = Number(value);
		if (Number.isFinite(n)) return n;
	}
	return undefined;
}

function clampPercent(value: number): number {
	return Math.min(100, Math.max(0, value));
}

function headerValue(
	headers: Record<string, string | number | boolean | null | undefined> | undefined,
	name: string,
): string | undefined {
	if (!headers) return undefined;
	const want = name.toLowerCase();
	for (const [key, value] of Object.entries(headers)) {
		if (key.toLowerCase() === want && value != null && value !== "") {
			return String(value);
		}
	}
	return undefined;
}

function quotaKind(provider: string | undefined): QuotaKind | undefined {
	if (provider === "openai-codex") return "codex";
	if (provider === "xai" || provider === "xai-auth") return "xai";
	return undefined;
}

function thinkingToken(level: string): string {
	return `thinking${level.charAt(0).toUpperCase()}${level.slice(1)}`;
}

function fillColor(percent: number): "success" | "warning" | "error" {
	if (percent >= 90) return "error";
	if (percent >= 70) return "warning";
	return "success";
}

function renderGauge(percent: number, width: number, theme: Theme): string {
	const cells = Math.max(1, width);
	const filled = Math.round((clampPercent(percent) / 100) * cells);
	return (
		theme.fg(fillColor(percent), "█".repeat(filled)) +
		theme.fg("dim", "░".repeat(cells - filled))
	);
}

function sessionStats(ctx: ExtensionContext): SessionStats {
	let cost = 0;
	let cacheHit: number | undefined;
	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type !== "message" || entry.message.role !== "assistant") continue;
		const usage = (entry.message as { usage?: Record<string, unknown> }).usage;
		if (!usage) continue;
		const total = asNumber(isRecord(usage.cost) ? usage.cost.total : undefined);
		if (total != null) cost += total;
		const input = asNumber(usage.input) ?? 0;
		const cacheRead = asNumber(usage.cacheRead) ?? 0;
		const cacheWrite = asNumber(usage.cacheWrite) ?? 0;
		const prompt = input + cacheRead + cacheWrite;
		if (prompt > 0) cacheHit = (cacheRead / prompt) * 100;
	}
	return { cost, cacheHit };
}

function parseCodexWindow(raw: unknown): QuotaWindow | undefined {
	if (!isRecord(raw)) return undefined;
	const used = asNumber(raw.used_percent);
	if (used == null) return undefined;
	return { usedPercent: clampPercent(used) };
}

function parseCodexPayload(payload: unknown): Omit<QuotaSnapshot, "kind" | "fetchedAt"> {
	const rateLimit = isRecord(payload) ? payload.rate_limit : undefined;
	if (!isRecord(rateLimit)) return {};
	return {
		fiveHour: parseCodexWindow(rateLimit.primary_window),
		weekly: parseCodexWindow(rateLimit.secondary_window),
	};
}

function parseCodexHeaders(
	headers: Record<string, string | number | boolean | null | undefined>,
): Omit<QuotaSnapshot, "kind" | "fetchedAt"> | undefined {
	const five = asNumber(headerValue(headers, "x-codex-primary-used-percent"));
	const week = asNumber(headerValue(headers, "x-codex-secondary-used-percent"));
	if (five == null && week == null) return undefined;
	return {
		fiveHour: five == null ? undefined : { usedPercent: clampPercent(five) },
		weekly: week == null ? undefined : { usedPercent: clampPercent(week) },
	};
}

function grokPeriodMinutes(config: Record<string, unknown>): number | undefined {
	const period = isRecord(config.currentPeriod) ? config.currentPeriod : undefined;
	const start = Date.parse(typeof period?.start === "string" ? period.start : "");
	const end = Date.parse(typeof period?.end === "string" ? period.end : "");
	if (!Number.isFinite(start) || !Number.isFinite(end) || end <= start) return undefined;
	return (end - start) / 60_000;
}

function parseGrokBilling(payload: unknown): Omit<QuotaSnapshot, "kind" | "fetchedAt"> {
	const config = isRecord(payload) && isRecord(payload.config) ? payload.config : undefined;
	if (!config) return {};
	let used = asNumber(config.creditUsagePercent);
	if (used == null) {
		const period = isRecord(config.currentPeriod) ? config.currentPeriod : undefined;
		const type = typeof period?.type === "string" ? period.type : "";
		if (type.toUpperCase().includes("WEEK") && config.used == null && config.monthlyLimit == null) {
			used = 0;
		}
	}
	if (used == null) return {};
	const window: QuotaWindow = { usedPercent: clampPercent(used) };
	const minutes = grokPeriodMinutes(config);
	if (minutes != null && minutes <= 6 * 60) return { fiveHour: window };
	return { weekly: window };
}

type ProviderAuth = {
	apiKey?: string;
	headers?: Record<string, string>;
};

async function providerAuth(
	ctx: ExtensionContext,
	provider: string,
): Promise<ProviderAuth | undefined> {
	const registry = ctx.modelRegistry as {
		isUsingOAuth?: (model: unknown) => boolean;
		getProviderAuth?: (id: string) => Promise<{ auth?: ProviderAuth } | undefined>;
	};
	if (ctx.model && registry.isUsingOAuth && !registry.isUsingOAuth(ctx.model)) {
		return undefined;
	}
	const result = await registry.getProviderAuth?.(provider);
	return result?.auth;
}

function authorization(auth: ProviderAuth | undefined): string | undefined {
	return headerValue(auth?.headers, "Authorization") ?? (auth?.apiKey ? `Bearer ${auth.apiKey}` : undefined);
}

async function fetchJson(
	url: string,
	headers: Record<string, string>,
	timeoutMs: number,
): Promise<unknown> {
	const controller = new AbortController();
	const timer = setTimeout(() => controller.abort(), timeoutMs);
	try {
		const response = await fetch(url, {
			headers: { Accept: "application/json", ...headers },
			signal: controller.signal,
			redirect: "error",
		});
		if (!response.ok) throw new Error(`HTTP ${response.status}`);
		return await response.json();
	} finally {
		clearTimeout(timer);
	}
}

async function fetchCodexQuota(ctx: ExtensionContext): Promise<QuotaSnapshot | undefined> {
	const auth = await providerAuth(ctx, "openai-codex");
	const token = authorization(auth);
	if (!token) return undefined;
	const payload = await fetchJson(
		CODEX_USAGE_URL,
		{ Authorization: token },
		QUOTA_TIMEOUT_MS,
	);
	return { kind: "codex", fetchedAt: Date.now(), ...parseCodexPayload(payload) };
}

function grokHeaders(userId?: string): Record<string, string> {
	return {
		"X-XAI-Token-Auth": "xai-grok-cli",
		"x-grok-client-version": GROK_CLIENT_VERSION,
		"x-grok-client-mode": "headless",
		...(userId ? { "x-userid": userId } : {}),
	};
}

async function fetchXaiQuota(ctx: ExtensionContext): Promise<QuotaSnapshot | undefined> {
	const provider = ctx.model?.provider === "xai-auth" ? "xai-auth" : "xai";
	const auth = await providerAuth(ctx, provider);
	const token = authorization(auth);
	if (!token) return undefined;
	const headers = { Authorization: token, ...grokHeaders() };
	let userId: string | undefined;
	try {
		const identity = await fetchJson(GROK_USER_URL, headers, QUOTA_TIMEOUT_MS);
		if (isRecord(identity) && typeof identity.userId === "string") userId = identity.userId;
	} catch {
		// Billing still works for some accounts without the identity probe.
	}
	const payload = await fetchJson(
		GROK_CREDITS_URL,
		{ Authorization: token, ...grokHeaders(userId) },
		QUOTA_TIMEOUT_MS,
	);
	return { kind: "xai", fetchedAt: Date.now(), ...parseGrokBilling(payload) };
}

function formatQuota(snapshot: QuotaSnapshot | undefined, theme: Theme): string {
	if (!snapshot) return "";
	const parts: string[] = [];
	if (snapshot.fiveHour) {
		const pct = Math.round(snapshot.fiveHour.usedPercent);
		parts.push(`${theme.fg("dim", "5H:")} ${theme.fg(fillColor(pct), `${pct}%`)}`);
	}
	if (snapshot.weekly) {
		const pct = Math.round(snapshot.weekly.usedPercent);
		parts.push(`${theme.fg("dim", "WK:")} ${theme.fg(fillColor(pct), `${pct}%`)}`);
	}
	return parts.join(theme.fg("dim", " | "));
}

function fitFooter(left: string[], right: string[], sep: string, width: number): string {
	const rightParts = right.filter(Boolean);
	const rightLine = rightParts.join(sep);
	const rightWidth = visibleWidth(rightLine);
	if (rightWidth >= width) return truncateToWidth(rightLine, width, "");

	const leftParts = left.filter(Boolean);
	if (leftParts.length === 0) return rightLine;

	const available = width - rightWidth - visibleWidth(sep);
	while (leftParts.length > 1 && visibleWidth(leftParts.join(sep)) > available) {
		leftParts.shift();
	}
	if (leftParts.length === 0) return rightLine;
	const leftLine = truncateToWidth(leftParts.join(sep), available, "");
	if (!leftLine) return rightLine;
	return leftLine + sep + rightLine;
}

export default function (pi: ExtensionAPI) {
	const quotaByKind: Partial<Record<QuotaKind, QuotaSnapshot>> = {};
	const quotaInFlight = new Set<QuotaKind>();
	let quotaTimer: ReturnType<typeof setInterval> | undefined;
	let requestRender: (() => void) | undefined;
	let unsubBranch: (() => void) | undefined;
	let agentStartedAt: number | undefined;
	let firstTokenAt: number | undefined;

	function currentQuota(ctx: ExtensionContext): QuotaSnapshot | undefined {
		const kind = quotaKind(ctx.model?.provider);
		return kind ? quotaByKind[kind] : undefined;
	}

	function stopQuotaTimer(): void {
		if (quotaTimer) clearInterval(quotaTimer);
		quotaTimer = undefined;
	}

	async function refreshQuota(ctx: ExtensionContext, force = false): Promise<void> {
		const kind = quotaKind(ctx.model?.provider);
		if (!kind) {
			requestRender?.();
			return;
		}
		const cached = quotaByKind[kind];
		if (!force && cached && Date.now() - cached.fetchedAt < QUOTA_TTL_MS) return;
		if (quotaInFlight.has(kind)) return;
		quotaInFlight.add(kind);
		try {
			const next = kind === "codex" ? await fetchCodexQuota(ctx) : await fetchXaiQuota(ctx);
			if (quotaKind(ctx.model?.provider) !== kind) return;
			if (next && (next.fiveHour || next.weekly)) quotaByKind[kind] = next;
		} catch {
			// Keep the last good snapshot for this provider.
		} finally {
			quotaInFlight.delete(kind);
			requestRender?.();
		}
	}

	function installFooter(ctx: ExtensionContext): void {
		stopQuotaTimer();
		unsubBranch?.();
		unsubBranch = undefined;

		if (ctx.mode !== "tui") return;

		ctx.ui.setFooter((tui: TuiHandle, theme: Theme, footerData: FooterData) => {
			const thisRender = () => tui.requestRender();
			requestRender = thisRender;
			unsubBranch = footerData.onBranchChange(thisRender);
			void refreshQuota(ctx);
			stopQuotaTimer();
			quotaTimer = setInterval(() => void refreshQuota(ctx), QUOTA_TTL_MS);

			return {
				invalidate() {},
				dispose() {
					stopQuotaTimer();
					unsubBranch?.();
					unsubBranch = undefined;
					if (requestRender === thisRender) requestRender = undefined;
				},
				render(width: number): string[] {
					const sep = theme.fg("dim", ` ${SEPARATOR} `);
					const cwd = theme.fg("dim", basename(ctx.cwd) || ctx.cwd);
					const branchName = footerData.getGitBranch();
					const branch = branchName
						? theme.fg("muted", `${GIT_BRANCH_ICON} ${branchName}`)
						: "";
					const modelId = ctx.model?.name || ctx.model?.id || "no-model";
					const level = pi.getThinkingLevel() || "off";
					const model = `${theme.fg("text", modelId)} ${theme.fg("dim", "[")}${theme.fg(thinkingToken(level), level)}${theme.fg("dim", "]")}`;

					const usage = ctx.getContextUsage();
					const percent = usage?.percent;
					let context = "";
					if (percent != null) {
						const gaugeWidth =
							width < 80 ? 4 : width < 110 ? 6 : GAUGE_WIDTH;
						context = `${renderGauge(percent, gaugeWidth, theme)} ${theme.fg("dim", `${Math.round(percent)}%`)}`;
					}

					const stats = sessionStats(ctx);
					const cache =
						stats.cacheHit != null
							? theme.fg(
									stats.cacheHit < 50 ? "warning" : "dim",
									`${CACHE_ICON} ${stats.cacheHit.toFixed(0)}%`,
								)
							: "";
					const cost = theme.fg("dim", `$${stats.cost.toFixed(2)}`);
					const limits = formatQuota(currentQuota(ctx), theme);
					const statusTexts = [...(footerData.getExtensionStatuses?.().values() ?? [])]
						.map((text) => text.replace(/\s+/g, " ").trim())
						.filter(Boolean);
					const statuses = statusTexts.length
						? theme.fg("dim", statusTexts.join(" "))
						: "";

					return [
						fitFooter(
							[cwd, branch],
							[model, context, cache, cost, limits, statuses],
							sep,
							width,
						),
					];
				},
			};
		});
	}

	pi.registerEntryRenderer<TurnStats>(TURN_STATS_TYPE, (entry, _opts, theme) => {
		const data = entry.data;
		const line = data ? formatTurnStats(data, theme) : "";
		return {
			invalidate() {},
			render: () => (line ? [line] : []),
		};
	});

	pi.on("session_start", (_event, ctx) => {
		installFooter(ctx);
	});

	pi.on("session_shutdown", () => {
		stopQuotaTimer();
		unsubBranch?.();
		unsubBranch = undefined;
		requestRender = undefined;
		agentStartedAt = undefined;
		firstTokenAt = undefined;
	});

	pi.on("model_select", (_event, ctx) => {
		requestRender?.();
		void refreshQuota(ctx);
	});
	pi.on("thinking_level_select", () => requestRender?.());
	pi.on("agent_settled", (_event, ctx) => {
		void refreshQuota(ctx);
		requestRender?.();
	});

	pi.on("agent_start", () => {
		agentStartedAt = Date.now();
		firstTokenAt = undefined;
	});

	pi.on("message_update", (event) => {
		if (firstTokenAt != null) return;
		const update = (event as { assistantMessageEvent?: { type?: string; delta?: string } })
			.assistantMessageEvent;
		if (!update) return;
		if (
			(update.type === "text_delta" ||
				update.type === "thinking_delta" ||
				update.type === "toolcall_delta") &&
			update.delta
		) {
			firstTokenAt = Date.now();
		}
	});

	pi.on("agent_end", (event, ctx) => {
		requestRender?.();
		if (ctx.mode !== "tui") {
			agentStartedAt = undefined;
			firstTokenAt = undefined;
			return;
		}
		const startedAt = agentStartedAt;
		const tokenAt = firstTokenAt;
		agentStartedAt = undefined;
		firstTokenAt = undefined;
		if (startedAt == null) return;

		let input = 0;
		let output = 0;
		let cacheRead = 0;
		let cacheWrite = 0;
		const messages = (event as { messages?: Array<{ role?: string; usage?: Record<string, unknown> }> })
			.messages ?? [];
		for (const message of messages) {
			if (message.role !== "assistant" || !message.usage) continue;
			input += asNumber(message.usage.input) ?? 0;
			output += asNumber(message.usage.output) ?? 0;
			cacheRead += asNumber(message.usage.cacheRead) ?? 0;
			cacheWrite += asNumber(message.usage.cacheWrite) ?? 0;
		}
		if (input + output + cacheRead + cacheWrite <= 0) return;

		const now = Date.now();
		const elapsedMs = Math.max(0, now - startedAt);
		const genMs = tokenAt != null ? Math.max(0, now - tokenAt) : elapsedMs;
		const tps = output > 0 && genMs > 0 ? output / (genMs / 1000) : undefined;
		try {
			pi.appendEntry<TurnStats>(TURN_STATS_TYPE, {
				input,
				cacheRead,
				cacheWrite,
				output,
				tps,
				elapsedMs,
			});
		} catch {
			// Ephemeral sessions have nothing to persist.
		}
	});

	pi.on("after_provider_response", (event, ctx) => {
		if (quotaKind(ctx.model?.provider) !== "codex") return;
		const parsed = parseCodexHeaders(
			(event as { headers?: Record<string, string | number | boolean | null | undefined> })
				.headers ?? {},
		);
		if (!parsed || (!parsed.fiveHour && !parsed.weekly)) return;
		quotaByKind.codex = { kind: "codex", fetchedAt: Date.now(), ...parsed };
		requestRender?.();
	});
}
