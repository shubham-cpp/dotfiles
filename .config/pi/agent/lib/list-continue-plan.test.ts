import assert from "node:assert/strict";
import test from "node:test";
import { applyActionToBuffer, parseLine, planListNewline } from "../extensions/list-continue/plan.ts";

function run(lines: string[], line: number, col: number) {
	const action = planListNewline(lines, line, col);
	if (action.type === "plain") return { lines, line, col, action };
	return { ...applyActionToBuffer({ lines, line, col }, action), action };
}

function assertTailKept(lines: string[], line: number, col: number, tail: string[]) {
	const result = run(lines, line, col);
	assert.deepEqual(result.lines.slice(result.lines.length - tail.length), tail);
	return result;
}

function insert(lines: string[], line: number, col: number): string {
	const action = planListNewline(lines, line, col);
	assert.equal(action.type, "insert");
	if (action.type !== "insert") throw new Error("expected insert");
	const current = lines[line] ?? "";
	return `${current.slice(0, col)}${action.text}${current.slice(col)}`;
}

function replace(lines: string[], line: number, col: number): string {
	const action = planListNewline(lines, line, col);
	assert.equal(action.type, "replaceLine");
	if (action.type !== "replaceLine") throw new Error("expected replaceLine");
	const next = lines.slice();
	next[line] = action.line;
	return next.join("\n");
}

test("unordered continue and split", () => {
	assert.equal(insert(["- something"], 0, "- something".length), "- something\n- ");
	assert.equal(insert(["* something"], 0, "* something".length), "* something\n* ");
	assert.equal(insert(["+ something"], 0, "+ something".length), "+ something\n+ ");
	assert.equal(insert(["- helloworld"], 0, "- hello".length), "- hello\n- world");
	assert.equal(insert(["    - nested"], 0, "    - nested".length), "    - nested\n    - ");
	assert.equal(insert(["- hello world"], 0, "- hello".length), "- hello\n-  world");
	assert.equal(insert(["- hello"], 0, 2), "- \n- hello");
});

test("ordered and letter increment", () => {
	assert.equal(insert(["1. foo"], 0, 6), "1. foo\n2. ");
	assert.equal(insert(["1) foo"], 0, 6), "1) foo\n2) ");
	assert.equal(insert(["9. foo"], 0, 6), "9. foo\n10. ");
	assert.equal(insert(["A. foo"], 0, 6), "A. foo\nB. ");
	assert.equal(insert(["a) foo"], 0, 6), "a) foo\nb) ");
	assert.equal(planListNewline(["Z. foo"], 0, 6).type, "plain");
	assert.equal(planListNewline(["z. foo"], 0, 6).type, "plain");
});

test("checkboxes reset to unchecked", () => {
	assert.equal(insert(["- [x] done"], 0, "- [x] done".length), "- [x] done\n- [ ] ");
	assert.equal(insert(["- [X] done"], 0, "- [X] done".length), "- [X] done\n- [ ] ");
	assert.equal(insert(["* [ ] todo"], 0, "* [ ] todo".length), "* [ ] todo\n* [ ] ");
});

test("empty item outdents or exits", () => {
	assert.equal(replace(["- "], 0, 2), "");
	assert.equal(replace(["-"], 0, 1), "");
	assert.equal(replace(["- [ ] "], 0, 6), "");
	assert.equal(replace(["- foo", "  - "], 1, 4), "- foo\n- ");
	assert.equal(replace(["- foo", "    - "], 1, 6), "- foo\n- ");
	assert.equal(replace(["1. foo", "   1. "], 1, 6), "1. foo\n2. ");
	assert.equal(replace(["1. foo", "   - "], 1, 5), "1. foo\n- ");
	assert.equal(replace(["A. foo", "   A. "], 1, 6), "A. foo\nB. ");
	assert.equal(replace(["- foo", "  - [ ] "], 1, 8), "- foo\n- [ ] ");
});

test("quotes continue and outdent after the list", () => {
	assert.equal(insert(["> - item"], 0, "> - item".length), "> - item\n> - ");
	assert.equal(replace(["> - "], 0, 4), "> ");
	assert.equal(replace(["> "], 0, 2), "");
	assert.equal(insert(["> hello"], 0, 7), "> hello\n> ");
	assert.equal(insert([">> nested"], 0, 9), ">> nested\n>> ");
	assert.equal(replace([">> "], 0, 3), "> ");
	assert.equal(replace(["> - foo", ">   - "], 1, 6), "> - foo\n> - ");
});

test("suppression: fences, thematic breaks, mid-line dashes, no space, cursor in prefix", () => {
	assert.equal(planListNewline(["```", "- item"], 1, 6).type, "plain");
	assert.equal(planListNewline(["~~~", "- item", "~~~", "- item"], 3, 6).type, "insert");
	assert.equal(planListNewline(["---"], 0, 3).type, "plain");
	assert.equal(planListNewline(["***"], 0, 3).type, "plain");
	assert.equal(planListNewline(["foo - bar"], 0, 9).type, "plain");
	assert.equal(planListNewline(["-item"], 0, 5).type, "plain");
	assert.equal(planListNewline(["- item"], 0, 0).type, "plain");
	assert.equal(planListNewline(["1.2 foo"], 0, 7).type, "plain");
});

test("unclosed fence opener pair-completes on newline", () => {
	assert.deepEqual(planListNewline(["```"], 0, 3), {
		type: "openFence",
		middle: "",
		closer: "```",
		cursorCol: 0,
	});
	assert.deepEqual(planListNewline(["```ts"], 0, 5), {
		type: "openFence",
		middle: "",
		closer: "```",
		cursorCol: 0,
	});
	assert.deepEqual(planListNewline(["~~~~"], 0, 4), {
		type: "openFence",
		middle: "",
		closer: "~~~~",
		cursorCol: 0,
	});
	assert.deepEqual(planListNewline(["  ```"], 0, 5), {
		type: "openFence",
		middle: "  ",
		closer: "  ```",
		cursorCol: 2,
	});
	assert.deepEqual(planListNewline(["> ```"], 0, 5), {
		type: "openFence",
		middle: "> ",
		closer: "> ```",
		cursorCol: 2,
	});
	assert.equal(planListNewline(["```", "code", "```"], 0, 3).type, "plain");
	assert.equal(planListNewline(["```", "already a body"], 0, 3).type, "plain");
	assert.equal(planListNewline(["```"], 0, 1).type, "plain");
	assert.equal(planListNewline(["foo ```"], 0, 7).type, "plain");
});

test("never drops lines after the cursor when the caret is in the middle", () => {
	const tail = ["keep this", "and the end"];

	const split = assertTailKept(["- hello world", ...tail], 0, "- hello".length, tail);
	assert.equal(split.action.type, "insert");
	assert.equal(split.lines[0], "- hello");
	assert.equal(split.lines[1], "-  world");

	const outdent = assertTailKept(["- foo", "  - ", ...tail], 1, 4, tail);
	assert.equal(outdent.action.type, "replaceLine");
	assert.equal(outdent.lines[1], "- ");

	const exit = assertTailKept(["- ", ...tail], 0, 2, tail);
	assert.equal(exit.action.type, "replaceLine");
	assert.equal(exit.lines[0], "");

	const quoted = assertTailKept(["> hello", ...tail], 0, 7, tail);
	assert.equal(quoted.action.type, "insert");

	const unclosed = run(["```"], 0, 3);
	assert.equal(unclosed.action.type, "openFence");
	assert.deepEqual(unclosed.lines, ["```", "", "```"]);

	const bodyBelow = run(["```", ...tail], 0, 3);
	assert.equal(bodyBelow.action.type, "plain");
	assert.deepEqual(bodyBelow.lines, ["```", ...tail]);

	const emptyThenTail = run(["```", "", ...tail], 0, 3);
	assert.equal(emptyThenTail.action.type, "plain");
	assert.deepEqual(emptyThenTail.lines, ["```", "", ...tail]);

	const closed = run(["```", "code", "```", ...tail], 0, 3);
	assert.equal(closed.action.type, "plain");
	assert.deepEqual(closed.lines, ["```", "code", "```", ...tail]);

	const midTicks = run(["```", ...tail], 0, 1);
	assert.equal(midTicks.action.type, "plain");
	assert.deepEqual(midTicks.lines, ["```", ...tail]);
});

test("parse requires a real list marker", () => {
	assert.equal(parseLine("- item")?.kind, "list");
	assert.equal(parseLine("1. item")?.kind, "list");
	assert.equal(parseLine("A. item")?.kind, "list");
	assert.equal(parseLine("> quote")?.kind, "quote");
	assert.equal(parseLine("plain"), null);
	assert.equal(parseLine("---"), null);
});
