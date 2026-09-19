import assert from "node:assert/strict";
import test from "node:test";
import { overlayName, paintTopBorder, trailingFillColumns, visibleWidth } from "./plan.ts";

function workingLine(width: number, overflow = 0): string {
	const status = "── ⠋ Working ";
	if (overflow <= 0) {
		return status + "─".repeat(Math.max(0, width - visibleWidth(status)));
	}
	const ov = ` ↑ ${overflow} more `;
	const rest = Math.max(0, width - visibleWidth(status) - visibleWidth(ov));
	const leftDashes = Math.floor(rest / 2);
	const rightDashes = rest - leftDashes;
	return status + "─".repeat(leftDashes) + ov + "─".repeat(rightDashes);
}

test("empty or blank names leave the border alone", () => {
	const line = workingLine(40);
	assert.equal(overlayName(line, "", (s) => s), line);
	assert.equal(overlayName(line, "   ", (s) => s), line);
});

test("paints the name on trailing fill and colors only that part", () => {
	const out = paintTopBorder((w) => "─".repeat(w), (s) => `[${s}]`, "Hello", 40);
	assert.equal(visibleWidth(out.replace(/[\[\]]/g, "")), 40);
	assert.ok(out.endsWith("[ Hello ]"));
	assert.ok(out.startsWith("─"));
});

test("always renders the left border at full width", () => {
	let seen = -1;
	paintTopBorder(
		(w) => {
			seen = w;
			return workingLine(w, 3);
		},
		(s) => s,
		"Hello",
		80,
	);
	assert.equal(seen, 80);
});

test("keeps Working and overflow when the name is long", () => {
	const out = paintTopBorder((w) => workingLine(w, 3), (s) => s, "A".repeat(80), 80);
	assert.match(out, /Working/);
	assert.match(out, /↑ 3 more/);
	assert.ok(out.includes("…"));
	assert.equal(visibleWidth(out), 80);
});

test("keeps Working and overflow in a 40-column pane", () => {
	const out = paintTopBorder((w) => workingLine(w, 3), (s) => s, "Hello", 40);
	assert.match(out, /Working/);
	assert.match(out, /↑ 3 more/);
	assert.ok(visibleWidth(out) <= 40);
});

test("drops the name when trailing fill cannot hold it", () => {
	const line = "── ⠋ Working ─ ↑ 3 more ─";
	assert.equal(trailingFillColumns(line), 1);
	assert.equal(overlayName(line, "Hello", (s) => s), line);
});

test("CJK names do not exceed the row width", () => {
	const out = paintTopBorder((w) => workingLine(w), (s) => s, "你好世界", 80);
	assert.ok(visibleWidth(out) <= 80);
	assert.match(out, /你好/);
});

test("emoji names do not eat the Working reserve", () => {
	const out = paintTopBorder((w) => workingLine(w), (s) => s, "🚀".repeat(20), 30);
	assert.match(out, /Working/);
	assert.ok(visibleWidth(out) <= 30);
});

test("strips ANSI from the session name before paint", () => {
	const out = overlayName("─".repeat(40), "\x1b[31mHello\x1b[0m", (s) => s);
	assert.equal(out.includes("\x1b[31m"), false);
	assert.match(out, /Hello/);
});

test("RGI emoji stay within terminal columns", () => {
	const out = overlayName("─".repeat(80), "⭐".repeat(40), (s) => s);
	let cols = 0;
	for (const { segment } of new Intl.Segmenter("en", { granularity: "grapheme" }).segment(out)) {
		cols += /^\p{RGI_Emoji}$/v.test(segment) ? 2 : 1;
	}
	assert.ok(cols <= 80);
});
