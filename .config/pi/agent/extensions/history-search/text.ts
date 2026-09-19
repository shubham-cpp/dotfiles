/** Collapse a stored prompt to a single display line. */
export function previewText(text: string): string {
	return text.replace(/\s+/g, " ").trim();
}

/**
 * Canonical stored form: CRLF to LF, strip per-line trailing space, trim.
 * Matches OMP HistoryStorage so resubmitted copies do not become duplicates.
 */
export function normalizePrompt(prompt: string): string {
	return prompt
		.replace(/\r\n?/g, "\n")
		.replace(/[^\S\n]+\n/g, "\n")
		.trim();
}

export function extractText(content: unknown): string | undefined {
	if (typeof content === "string") {
		const text = content.trim();
		return text.length > 0 ? content : undefined;
	}
	if (!Array.isArray(content)) return undefined;

	const parts: string[] = [];
	for (const block of content) {
		if (!block || typeof block !== "object") continue;
		const rec = block as { type?: unknown; text?: unknown };
		if (rec.type === "text" && typeof rec.text === "string" && rec.text.trim()) {
			parts.push(rec.text);
		}
	}
	if (parts.length === 0) return undefined;
	return parts.join("\n");
}

export function isHistoryPrompt(text: string): boolean {
	const trimmed = text.trim();
	if (!trimmed) return false;
	if (trimmed.startsWith("/")) return false;
	return true;
}

/** Split on non-alphanumeric runs, same tokenizer OMP uses for FTS alignment. */
export function queryTokens(query: string): string[] {
	return query
		.toLowerCase()
		.split(/[^\p{L}\p{N}]+/u)
		.filter((tok) => tok.length > 0);
}

export function matchesQuery(prompt: string, tokens: string[]): boolean {
	if (tokens.length === 0) return true;
	const lower = prompt.toLowerCase();
	return tokens.every((tok) => lower.includes(tok));
}

/** Wrap every case-insensitive occurrence of any token with the highlight fn. */
export function highlightTokens(
	text: string,
	tokens: string[],
	highlight: (chunk: string) => string,
): string {
	if (tokens.length === 0) return text;

	const lower = text.toLowerCase();
	const ranges: Array<[number, number]> = [];
	for (const tok of tokens) {
		let from = lower.indexOf(tok);
		while (from !== -1) {
			ranges.push([from, from + tok.length]);
			from = lower.indexOf(tok, from + tok.length);
		}
	}
	if (ranges.length === 0) return text;

	ranges.sort((a, b) => a[0] - b[0]);
	let out = "";
	let pos = 0;
	for (const [start, end] of ranges) {
		if (end <= pos) continue;
		const from = Math.max(start, pos);
		if (from > pos) out += text.slice(pos, from);
		out += highlight(text.slice(from, end));
		pos = end;
	}
	if (pos < text.length) out += text.slice(pos);
	return out;
}

export function relativeTime(epochMs: number): string {
	const seconds = Math.max(0, Math.floor((Date.now() - epochMs) / 1000));
	if (seconds < 60) return "now";
	const minutes = Math.floor(seconds / 60);
	if (minutes < 60) return `${minutes}m`;
	const hours = Math.floor(minutes / 60);
	if (hours < 24) return `${hours}h`;
	const days = Math.floor(hours / 24);
	if (days < 7) return `${days}d`;
	if (days < 30) return `${Math.floor(days / 7)}w`;
	if (days < 365) return `${Math.floor(days / 30)}mo`;
	return `${Math.floor(days / 365)}y`;
}

export function timestampMs(entryTs: unknown, messageTs: unknown): number {
	const fromMessage = asTime(messageTs);
	if (fromMessage != null) return fromMessage;
	const fromEntry = asTime(entryTs);
	if (fromEntry != null) return fromEntry;
	return Date.now();
}

function asTime(value: unknown): number | undefined {
	if (typeof value === "number" && Number.isFinite(value)) {
		return value < 1e12 ? value * 1000 : value;
	}
	if (typeof value === "string" && value.trim()) {
		const parsed = Date.parse(value);
		if (Number.isFinite(parsed)) return parsed;
	}
	return undefined;
}
