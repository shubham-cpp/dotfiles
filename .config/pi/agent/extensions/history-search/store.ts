import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { SessionManager, getAgentDir } from "@earendil-works/pi-coding-agent";

import {
	extractText,
	isHistoryPrompt,
	matchesQuery,
	normalizePrompt,
	queryTokens,
	timestampMs,
} from "./text.ts";

export type HistoryScope = "local" | "all";

export type HistoryEntry = {
	prompt: string;
	createdAt: number;
	cwd?: string;
};

type StoredFile = {
	version: 1;
	backfilled: boolean;
	entries: HistoryEntry[];
};

type SessionLike = {
	path?: string;
	file?: string;
	cwd?: string;
	modified?: Date | string | number;
};

type SessionManagerLike = {
	open: (path: string) => {
		getEntries: () => Array<{
			type?: string;
			timestamp?: unknown;
			message?: { role?: unknown; content?: unknown; timestamp?: unknown };
		}>;
		getCwd?: () => string;
		getHeader?: () => { cwd?: string };
	};
	list: (cwd: string) => Promise<SessionLike[]>;
	listAll?: (...args: unknown[]) => Promise<SessionLike[]>;
};

const STORE_VERSION = 1;
const MAX_ENTRIES = 1000;
const MAX_PROMPT_CHARS = 16_384;
const MAX_SESSIONS_SCAN = 250;

export class HistoryStore {
	private entries: HistoryEntry[] = [];
	private index = new Map<string, HistoryEntry>();
	private backfilled = false;
	private dirty = false;
	private backfillPromise: Promise<void> | undefined;
	private readonly filePath: string;

	private constructor(filePath: string) {
		this.filePath = filePath;
		this.load();
	}

	static open(): HistoryStore {
		return new HistoryStore(join(getAgentDir(), "history-search.json"));
	}

	isBackfilled(): boolean {
		return this.backfilled;
	}

	add(prompt: string, cwd?: string, createdAt = Date.now()): void {
		const normalized = normalizePrompt(prompt);
		if (!isHistoryPrompt(normalized)) return;
		if (normalized.length > MAX_PROMPT_CHARS) return;

		const existing = this.index.get(normalized);
		if (existing) {
			if (createdAt >= existing.createdAt) {
				existing.createdAt = createdAt;
				if (cwd) existing.cwd = cwd;
				this.moveToFront(existing);
				this.dirty = true;
				this.persist();
			}
			return;
		}

		const entry: HistoryEntry = { prompt: normalized, createdAt, cwd };
		this.entries.unshift(entry);
		this.index.set(normalized, entry);
		if (this.entries.length > MAX_ENTRIES) {
			const dropped = this.entries.pop();
			if (dropped) this.index.delete(dropped.prompt);
		}
		this.dirty = true;
		this.persist();
	}

	ingestMany(items: Array<{ prompt: string; cwd?: string; createdAt?: number }>): void {
		let changed = false;
		for (const item of items) {
			if (this.ingestSilent(item.prompt, item.cwd, item.createdAt ?? Date.now())) {
				changed = true;
			}
		}
		if (changed) {
			this.sortNewestFirst();
			this.dirty = true;
			this.persist();
		}
	}

	search(query: string, scope: HistoryScope, cwd: string): HistoryEntry[] {
		const tokens = queryTokens(query);
		const out: HistoryEntry[] = [];
		for (const entry of this.entries) {
			if (scope === "local" && entry.cwd && entry.cwd !== cwd) continue;
			if (!matchesQuery(entry.prompt, tokens)) continue;
			out.push(entry);
		}
		return out;
	}

	ensureBackfill(cwd: string): Promise<void> {
		if (this.backfilled) return Promise.resolve();
		if (this.backfillPromise) return this.backfillPromise;
		this.backfillPromise = this.backfillFromSessions(cwd).finally(() => {
			this.backfillPromise = undefined;
		});
		return this.backfillPromise;
	}

	private ingestSilent(prompt: string, cwd: string | undefined, createdAt: number): boolean {
		const normalized = normalizePrompt(prompt);
		if (!isHistoryPrompt(normalized)) return false;
		if (normalized.length > MAX_PROMPT_CHARS) return false;

		const existing = this.index.get(normalized);
		if (existing) {
			if (createdAt > existing.createdAt) {
				existing.createdAt = createdAt;
				if (cwd) existing.cwd = cwd;
				this.moveToFront(existing);
				return true;
			}
			if (cwd && !existing.cwd) {
				existing.cwd = cwd;
				return true;
			}
			return false;
		}

		const entry: HistoryEntry = { prompt: normalized, createdAt, cwd };
		this.entries.push(entry);
		this.index.set(normalized, entry);
		return true;
	}

	private moveToFront(entry: HistoryEntry): void {
		const idx = this.entries.indexOf(entry);
		if (idx > 0) {
			this.entries.splice(idx, 1);
			this.entries.unshift(entry);
		}
	}

	private async backfillFromSessions(cwd: string): Promise<void> {
		const sessions = await listSessions(cwd);
		sessions.sort((a, b) => sessionModified(b) - sessionModified(a));
		const limited = sessions.slice(0, MAX_SESSIONS_SCAN);

		const items: Array<{ prompt: string; cwd?: string; createdAt: number }> = [];
		for (const session of limited) {
			const path = session.path ?? session.file;
			if (!path) continue;
			items.push(...readSessionPrompts(path, session.cwd));
		}

		this.ingestMany(items);
		this.sortNewestFirst();
		this.backfilled = true;
		this.dirty = true;
		this.persist();
	}

	private sortNewestFirst(): void {
		this.entries.sort((a, b) => b.createdAt - a.createdAt || b.prompt.localeCompare(a.prompt));
		if (this.entries.length > MAX_ENTRIES) {
			for (const dropped of this.entries.splice(MAX_ENTRIES)) {
				this.index.delete(dropped.prompt);
			}
		}
	}

	private load(): void {
		try {
			if (!existsSync(this.filePath)) return;
			const raw = JSON.parse(readFileSync(this.filePath, "utf8")) as StoredFile;
			if (raw.version !== STORE_VERSION || !Array.isArray(raw.entries)) return;
			this.backfilled = Boolean(raw.backfilled);
			for (const entry of raw.entries) {
				if (!entry || typeof entry.prompt !== "string") continue;
				if (typeof entry.createdAt !== "number" || !Number.isFinite(entry.createdAt)) continue;
				this.ingestSilent(
					entry.prompt,
					typeof entry.cwd === "string" ? entry.cwd : undefined,
					entry.createdAt,
				);
			}
			this.sortNewestFirst();
			this.dirty = false;
		} catch {
			this.entries = [];
			this.index = new Map();
			this.backfilled = false;
		}
	}

	private persist(): void {
		if (!this.dirty) return;
		const payload: StoredFile = {
			version: STORE_VERSION,
			backfilled: this.backfilled,
			entries: this.entries,
		};
		try {
			mkdirSync(dirname(this.filePath), { recursive: true });
			const tmp = `${this.filePath}.${process.pid}.tmp`;
			writeFileSync(tmp, `${JSON.stringify(payload)}\n`);
			renameSync(tmp, this.filePath);
			this.dirty = false;
		} catch {
			// Keep working from memory if disk is unavailable.
		}
	}
}

async function listSessions(cwd: string): Promise<SessionLike[]> {
	const manager = SessionManager as unknown as SessionManagerLike;
	if (typeof manager.listAll === "function") {
		for (const args of [[], [cwd]] as unknown[][]) {
			try {
				const result = await manager.listAll(...args);
				if (Array.isArray(result)) return result;
			} catch {
				// Older/newer signatures differ on the first argument.
			}
		}
	}
	try {
		return await manager.list(cwd);
	} catch {
		return [];
	}
}

function sessionModified(session: SessionLike): number {
	const modified = session.modified;
	if (modified instanceof Date) return modified.getTime();
	if (typeof modified === "number" && Number.isFinite(modified)) return modified;
	if (typeof modified === "string") {
		const parsed = Date.parse(modified);
		if (Number.isFinite(parsed)) return parsed;
	}
	return 0;
}

function readSessionPrompts(
	path: string,
	fallbackCwd?: string,
): Array<{ prompt: string; cwd?: string; createdAt: number }> {
	try {
		const manager = (SessionManager as unknown as SessionManagerLike).open(path);
		const cwd = manager.getCwd?.() ?? manager.getHeader?.()?.cwd ?? fallbackCwd;
		const items: Array<{ prompt: string; cwd?: string; createdAt: number }> = [];
		for (const entry of manager.getEntries()) {
			if (entry.type !== "message") continue;
			const message = entry.message;
			if (!message || message.role !== "user") continue;
			const text = extractText(message.content);
			if (!text) continue;
			items.push({
				prompt: text,
				cwd,
				createdAt: timestampMs(entry.timestamp, message.timestamp),
			});
		}
		return items;
	} catch {
		return [];
	}
}
