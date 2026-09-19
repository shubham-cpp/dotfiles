const ANSI = /\x1b\[[0-9;]*m/g;
const FILL = "─";
const MIN_LABEL = 3;
const MIN_GAP = 1;

const segmenter = new Intl.Segmenter("en", { granularity: "grapheme" });

function graphemeWidth(g: string): number {
	const cp = g.codePointAt(0) ?? 0;
	if (cp <= 0x1f || (cp >= 0x7f && cp <= 0x9f)) return 0;
	if (cp >= 0x300 && cp <= 0x36f) return 0;
	if (/^\p{RGI_Emoji}$/v.test(g)) return 2;
	if (
		cp >= 0x1100 &&
		(cp <= 0x115f ||
			cp === 0x2329 ||
			cp === 0x232a ||
			(cp >= 0x2e80 && cp <= 0xa4cf && cp !== 0x303f) ||
			(cp >= 0xac00 && cp <= 0xd7a3) ||
			(cp >= 0xf900 && cp <= 0xfaff) ||
			(cp >= 0xfe10 && cp <= 0xfe19) ||
			(cp >= 0xfe30 && cp <= 0xfe6f) ||
			(cp >= 0xff00 && cp <= 0xff60) ||
			(cp >= 0xffe0 && cp <= 0xffe6) ||
			(cp >= 0x1f300 && cp <= 0x1faff) ||
			cp >= 0x20000)
	) {
		return 2;
	}
	return [...g].length > 1 ? 2 : 1;
}

export function visibleWidth(text: string): number {
	let width = 0;
	for (const { segment } of segmenter.segment(text.replace(ANSI, ""))) {
		width += graphemeWidth(segment);
	}
	return width;
}

function truncateToWidth(text: string, width: number, ellipsis = "…"): string {
	if (width <= 0) return "";
	if (visibleWidth(text) <= width) return text;
	const budget = Math.max(0, width - visibleWidth(ellipsis));
	let out = "";
	let used = 0;
	for (const part of text.split(/(\x1b\[[0-9;]*m)/)) {
		if (part.startsWith("\x1b")) {
			out += part;
			continue;
		}
		for (const { segment } of segmenter.segment(part)) {
			const w = graphemeWidth(segment);
			if (used + w > budget) return out + ellipsis;
			out += segment;
			used += w;
		}
	}
	return out + ellipsis;
}

export function trailingFillColumns(line: string): number {
	const stripped = line.replace(ANSI, "");
	let n = 0;
	for (let i = stripped.length - 1; i >= 0; i--) {
		if (stripped[i] !== FILL) break;
		n += 1;
	}
	return n;
}

function sanitizeName(name: string): string {
	return name.replace(ANSI, "").replace(/[\x00-\x1f\x7f]/g, "").trim();
}

export function overlayName(line: string, name: string, color: (text: string) => string): string {
	const trimmed = sanitizeName(name);
	if (!trimmed) return line;
	const budget = trailingFillColumns(line) - MIN_GAP;
	if (budget < MIN_LABEL) return line;
	const label = truncateToWidth(` ${trimmed} `, budget);
	const labelW = visibleWidth(label);
	if (labelW === 0) return line;
	return truncateToWidth(line, visibleWidth(line) - labelW, "") + color(label);
}

export function paintTopBorder(
	renderLeft: (width: number) => string,
	color: (text: string) => string,
	name: string,
	width: number,
): string {
	const line = renderLeft(width);
	if (width <= 0) return line;
	return overlayName(line, name, color);
}
