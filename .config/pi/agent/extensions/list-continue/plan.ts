// Markdown list/quote continuation planner for the pi editor.
// Shift+Enter / Ctrl+J (tui.input.newLine) call this; Enter still submits.
// Empty items outdent one level (exit at the top). Unclosed ``` / ~~~
// openers pair-complete. No Tab/Shift+Tab. No soft-break.

export type Marker =
	| { type: "ul"; bullet: "-" | "*" | "+" }
	| { type: "ol"; n: number; delim: "." | ")" }
	| { type: "letter"; ch: string; delim: "." | ")" };

export type Action =
	| { type: "plain" }
	| { type: "insert"; text: string }
	| { type: "replaceLine"; line: string; col: number }
	| { type: "openFence"; middle: string; closer: string; cursorCol: number };

export type Buffer = {
	lines: string[];
	line: number;
	col: number;
};

/** Apply a planned newline. Never deletes lines after the cursor line. */
export function applyActionToBuffer(buf: Buffer, action: Action): Buffer {
	if (action.type === "plain") return buf;

	const current = buf.lines[buf.line] ?? "";
	const before = current.slice(0, buf.col);
	const after = current.slice(buf.col);
	const tail = buf.lines.slice(buf.line + 1);

	if (action.type === "insert") {
		const parts = action.text.split("\n");
		if (parts.length === 1) {
			const lines = buf.lines.slice();
			lines[buf.line] = before + action.text + after;
			return { lines, line: buf.line, col: buf.col + action.text.length };
		}
		const last = parts[parts.length - 1] ?? "";
		return {
			lines: [...buf.lines.slice(0, buf.line), before + (parts[0] ?? ""), ...parts.slice(1, -1), last + after, ...tail],
			line: buf.line + parts.length - 1,
			col: last.length,
		};
	}

	if (action.type === "openFence") {
		return {
			lines: [...buf.lines.slice(0, buf.line + 1), action.middle, action.closer, ...tail],
			line: buf.line + 1,
			col: action.cursorCol,
		};
	}

	const lines = buf.lines.slice();
	lines[buf.line] = action.line + after;
	return { lines, line: buf.line, col: action.col };
}

type ParsedList = {
	kind: "list";
	quote: string;
	indent: string;
	marker: Marker;
	checkbox: boolean;
	prefixLength: number;
};

type ParsedQuote = {
	kind: "quote";
	quote: string;
	indent: string;
	prefixLength: number;
};

type Parsed = ParsedList | ParsedQuote;

const FENCE_RE = /^\s{0,3}(`{3,}|~{3,})/;
const QUOTE_RE = /^(> ?)+/;

function indentWidth(indent: string): number {
	let width = 0;
	for (const ch of indent) width += ch === "\t" ? 4 : 1;
	return width;
}

function normQuote(quote: string): string {
	return quote.replaceAll(" ", "");
}

function stripOneQuote(quote: string): string {
	return quote.replace(/^> ?/, "");
}

function fenceToken(line: string): string | null {
	const stripped = line.replace(QUOTE_RE, "");
	const match = stripped.match(FENCE_RE);
	return match?.[1] ?? null;
}

type ParsedFence = {
	quote: string;
	indent: string;
	token: string;
	prefixLength: number;
};

export function parseFenceLine(line: string): ParsedFence | null {
	const quote = line.match(QUOTE_RE)?.[0] ?? "";
	const rest = line.slice(quote.length);
	const match = rest.match(/^([ \t]{0,3})(`{3,}|~{3,})(.*)$/);
	if (!match) return null;
	const indent = match[1] ?? "";
	const token = match[2] ?? "";
	const info = match[3] ?? "";
	if (token.startsWith("`") && info.includes("`")) return null;
	return {
		quote,
		indent,
		token,
		prefixLength: quote.length + indent.length + token.length,
	};
}

function fenceAlreadyClosed(lines: string[], openerIndex: number, token: string): boolean {
	for (let i = openerIndex + 1; i < lines.length; i++) {
		const closer = fenceToken(lines[i] ?? "");
		if (!closer) continue;
		if (closer[0] === token[0] && closer.length >= token.length) return true;
	}
	return false;
}

function hasBodyAfter(lines: string[], lineIndex: number): boolean {
	for (let i = lineIndex + 1; i < lines.length; i++) {
		if ((lines[i] ?? "").trim() !== "") return true;
	}
	return false;
}

function planFenceClose(lines: string[], line: number, col: number): Action | null {
	const currentLine = lines[line] ?? "";
	const fence = parseFenceLine(currentLine);
	if (!fence) return null;
	if (col < fence.prefixLength) return { type: "plain" };
	if (currentLine.slice(col).trim() !== "") return { type: "plain" };
	if (fenceAlreadyClosed(lines, line, fence.token)) return { type: "plain" };
	if (hasBodyAfter(lines, line)) return { type: "plain" };
	const middle = `${fence.quote}${fence.indent}`;
	return {
		type: "openFence",
		middle,
		closer: `${middle}${fence.token}`,
		cursorCol: middle.length,
	};
}

export function inFence(lines: string[], lineIndex: number): boolean {
	let open: string | null = null;
	for (let i = 0; i < lineIndex; i++) {
		const token = fenceToken(lines[i] ?? "");
		if (!token) continue;
		if (!open) {
			open = token;
		} else if (token[0] === open[0] && token.length >= open.length) {
			open = null;
		}
	}
	return open !== null;
}

function isThematicBreak(afterQuote: string): boolean {
	const trimmed = afterQuote.trim();
	if (trimmed.length < 3) return false;
	const compact = trimmed.replaceAll(" ", "");
	return /^([-*_])\1{2,}$/.test(compact);
}

function parseMarker(afterIndent: string): { marker: Marker; markerLen: number } | null {
	const ul = afterIndent.match(/^([-*+])(?= |$)/);
	if (ul) {
		return { marker: { type: "ul", bullet: ul[1] as "-" | "*" | "+" }, markerLen: 1 };
	}
	const ol = afterIndent.match(/^(\d+)([.)])(?= |$)/);
	if (ol) {
		return {
			marker: { type: "ol", n: Number(ol[1]), delim: ol[2] as "." | ")" },
			markerLen: ol[1]!.length + 1,
		};
	}
	const letter = afterIndent.match(/^([A-Za-z])([.)])(?= |$)/);
	if (letter) {
		return {
			marker: { type: "letter", ch: letter[1]!, delim: letter[2] as "." | ")" },
			markerLen: 2,
		};
	}
	return null;
}

export function parseLine(line: string): Parsed | null {
	const quoteMatch = line.match(QUOTE_RE);
	const quote = quoteMatch?.[0] ?? "";
	const afterQuote = line.slice(quote.length);
	if (isThematicBreak(afterQuote)) {
		if (!quote) return null;
		return { kind: "quote", quote, indent: "", prefixLength: quote.length };
	}

	const indentMatch = afterQuote.match(/^[ \t]*/);
	const indent = indentMatch?.[0] ?? "";
	const afterIndent = afterQuote.slice(indent.length);
	const parsedMarker = parseMarker(afterIndent);
	if (!parsedMarker) {
		if (!quote) return null;
		return {
			kind: "quote",
			quote,
			indent,
			prefixLength: quote.length + indent.length,
		};
	}

	const afterMarker = afterIndent.slice(parsedMarker.markerLen);
	if (afterMarker.length > 0 && !afterMarker.startsWith(" ")) return null;

	const spaceMatch = afterMarker.match(/^ +/);
	const spaces = spaceMatch?.[0] ?? "";
	const afterSpaces = afterMarker.slice(spaces.length);
	const taskMatch = afterSpaces.match(/^\[([ xX])\]( +|$)/);
	const checkbox = Boolean(taskMatch);
	const taskLen = taskMatch?.[0].length ?? 0;

	return {
		kind: "list",
		quote,
		indent,
		marker: parsedMarker.marker,
		checkbox,
		prefixLength: quote.length + indent.length + parsedMarker.markerLen + spaces.length + taskLen,
	};
}

function formatMarker(marker: Marker): string {
	if (marker.type === "ul") return marker.bullet;
	if (marker.type === "ol") return `${marker.n}${marker.delim}`;
	return `${marker.ch}${marker.delim}`;
}

export function formatPrefix(quote: string, indent: string, marker: Marker, checkbox: boolean): string {
	return `${quote}${indent}${formatMarker(marker)} ${checkbox ? "[ ] " : ""}`;
}

export function incrementMarker(marker: Marker): Marker | null {
	if (marker.type === "ul") return marker;
	if (marker.type === "ol") return { ...marker, n: marker.n + 1 };
	const code = marker.ch.charCodeAt(0);
	if (code >= 65 && code < 90) return { ...marker, ch: String.fromCharCode(code + 1) };
	if (code >= 97 && code < 122) return { ...marker, ch: String.fromCharCode(code + 1) };
	return null;
}

function sameFamily(a: Marker, b: Marker): boolean {
	if (a.type !== b.type) return false;
	if (a.type === "letter" && b.type === "letter") {
		const aUpper = a.ch === a.ch.toUpperCase();
		const bUpper = b.ch === b.ch.toUpperCase();
		return aUpper === bUpper;
	}
	return true;
}

function startMarker(marker: Marker): Marker {
	if (marker.type === "ul") return marker;
	if (marker.type === "ol") return { type: "ol", n: 1, delim: marker.delim };
	return {
		type: "letter",
		ch: marker.ch === marker.ch.toUpperCase() ? "A" : "a",
		delim: marker.delim,
	};
}

function findParentList(lines: string[], lineIndex: number, current: ParsedList): ParsedList | null {
	const quote = normQuote(current.quote);
	const width = indentWidth(current.indent);
	for (let i = lineIndex - 1; i >= 0; i--) {
		const parsed = parseLine(lines[i] ?? "");
		if (!parsed || parsed.kind !== "list") continue;
		if (normQuote(parsed.quote) !== quote) continue;
		if (indentWidth(parsed.indent) < width) return parsed;
	}
	return null;
}

function nextMarkerAtIndent(
	lines: string[],
	lineIndex: number,
	current: ParsedList,
	targetIndent: string,
): Marker | null {
	const quote = normQuote(current.quote);
	const width = indentWidth(targetIndent);
	for (let i = lineIndex - 1; i >= 0; i--) {
		const parsed = parseLine(lines[i] ?? "");
		if (!parsed || parsed.kind !== "list") continue;
		if (normQuote(parsed.quote) !== quote) continue;
		const parsedWidth = indentWidth(parsed.indent);
		if (parsedWidth < width) break;
		if (parsedWidth !== width) continue;
		if (!sameFamily(parsed.marker, current.marker)) continue;
		if (current.marker.type === "ul") return current.marker;
		return incrementMarker(parsed.marker);
	}
	if (current.marker.type === "ul") return current.marker;
	return startMarker(current.marker);
}

export function planListNewline(lines: string[], line: number, col: number): Action {
	if (inFence(lines, line)) return { type: "plain" };

	const fenceClose = planFenceClose(lines, line, col);
	if (fenceClose) return fenceClose;

	const currentLine = lines[line] ?? "";
	const parsed = parseLine(currentLine);
	if (!parsed) return { type: "plain" };
	if (col < parsed.prefixLength) return { type: "plain" };

	const empty =
		currentLine.slice(parsed.prefixLength, col).trim() === "" &&
		currentLine.slice(col).trim() === "";

	if (parsed.kind === "quote") {
		if (empty) {
			const nextQuote = stripOneQuote(parsed.quote);
			return { type: "replaceLine", line: nextQuote, col: nextQuote.length };
		}
		return { type: "insert", text: `\n${parsed.quote}${parsed.indent}` };
	}

	if (empty) {
		if (indentWidth(parsed.indent) > 0) {
			const parent = findParentList(lines, line, parsed);
			const targetIndent = parent?.indent ?? "";
			const nextMarker = nextMarkerAtIndent(lines, line, parsed, targetIndent);
			if (!nextMarker) {
				return { type: "replaceLine", line: parsed.quote, col: parsed.quote.length };
			}
			const nextLine = formatPrefix(parsed.quote, targetIndent, nextMarker, parsed.checkbox);
			return { type: "replaceLine", line: nextLine, col: nextLine.length };
		}
		return { type: "replaceLine", line: parsed.quote, col: parsed.quote.length };
	}

	const nextMarker = incrementMarker(parsed.marker);
	if (!nextMarker) return { type: "plain" };
	return {
		type: "insert",
		text: `\n${formatPrefix(parsed.quote, parsed.indent, nextMarker, parsed.checkbox)}`,
	};
}
