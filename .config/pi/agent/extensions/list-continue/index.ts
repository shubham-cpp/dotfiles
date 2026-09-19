// Continue markdown lists and quotes on tui.input.newLine (Shift+Enter, Ctrl+J).
// Unclosed ``` / ~~~ openers insert a matching closer with the caret in between.
// Enter still submits. Empty items outdent one level; Tab/Shift+Tab are untouched.

import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { applyActionToBuffer, planListNewline } from "./plan.ts";

const ATTACHED = Symbol.for("pi.list-continue");

type Keybindings = {
	matches: (data: string, action: string) => boolean;
};

type ListEditor = {
	getLines: () => string[];
	getCursor: () => { line: number; col: number };
	insertTextAtCursor: (text: string) => void;
	handleInput: (data: string) => void;
	getText: () => string;
	tui?: { requestRender: () => void };
};

type EditorInternals = {
	state?: { lines: string[]; cursorLine: number; cursorCol: number };
	pushUndoSnapshot?: () => void;
	onChange?: (text: string) => void;
	isInPaste?: boolean;
	jumpMode?: string | null;
	lastAction?: string | null;
};

function isListEditor(value: unknown): value is ListEditor {
	if (!value || typeof value !== "object") return false;
	const editor = value as ListEditor;
	return (
		typeof editor.getLines === "function" &&
		typeof editor.getCursor === "function" &&
		typeof editor.insertTextAtCursor === "function" &&
		typeof editor.handleInput === "function"
	);
}

function isNewline(data: string, keybindings: Keybindings): boolean {
	return keybindings.matches(data, "tui.input.newLine");
}

function applyAction(editor: ListEditor, action: Exclude<ReturnType<typeof planListNewline>, { type: "plain" }>): void {
	if (action.type === "insert") {
		editor.insertTextAtCursor(action.text);
		return;
	}

	const { line, col } = editor.getCursor();
	const next = applyActionToBuffer({ lines: editor.getLines(), line, col }, action);

	const internals = editor as unknown as EditorInternals;
	if (internals.state?.lines && typeof internals.pushUndoSnapshot === "function") {
		internals.pushUndoSnapshot();
		internals.state.lines = next.lines;
		internals.state.cursorLine = next.line;
		internals.state.cursorCol = next.col;
		internals.lastAction = null;
		internals.onChange?.(next.lines.join("\n"));
		editor.tui?.requestRender();
		return;
	}

	// Never backspace as a fallback: that can merge into the previous line.
	if (action.type === "openFence") {
		editor.insertTextAtCursor(`\n${action.middle}\n${action.closer}`);
		return;
	}
	editor.insertTextAtCursor(`\n${action.line}`);
}

function attachListContinue(editor: ListEditor, keybindings: Keybindings): void {
	const marked = editor as ListEditor & { [ATTACHED]?: boolean };
	if (marked[ATTACHED]) return;
	marked[ATTACHED] = true;

	const origHandleInput = editor.handleInput.bind(editor);
	editor.handleInput = (data: string) => {
		const internals = editor as unknown as EditorInternals;
		if (internals.isInPaste || internals.jumpMode) {
			origHandleInput(data);
			return;
		}
		if (!isNewline(data, keybindings)) {
			origHandleInput(data);
			return;
		}

		const { line, col } = editor.getCursor();
		const action = planListNewline(editor.getLines(), line, col);
		if (action.type === "plain") {
			origHandleInput(data);
			return;
		}
		applyAction(editor, action);
	};
}

export default function (pi: ExtensionAPI): void {
	pi.on("session_start", (_event, ctx) => {
		const previous = ctx.ui.getEditorComponent();
		ctx.ui.setEditorComponent((tui, theme, keybindings) => {
			const inner = previous?.(tui, theme, keybindings);
			const editor = isListEditor(inner)
				? inner
				: new CustomEditor(tui, theme, keybindings, { embedWorkingStatus: true });
			attachListContinue(editor, keybindings);
			return editor;
		});
	});
}
