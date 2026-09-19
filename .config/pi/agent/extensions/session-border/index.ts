// Show the session name on the right of the editor top border.
// Overlay on trailing dashes so Working / overflow stay put.

import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { paintTopBorder } from "./plan.ts";

const ATTACHED = Symbol.for("pi.session-border");

type SessionBorderEditor = {
	renderTopBorder: (width: number, hiddenLineCount: number) => string;
	borderColor: (text: string) => string;
	[ATTACHED]?: boolean;
};

function isSessionBorderEditor(value: unknown): value is SessionBorderEditor {
	if (!value || typeof value !== "object") return false;
	const editor = value as SessionBorderEditor;
	return typeof editor.renderTopBorder === "function" && typeof editor.borderColor === "function";
}

function attachSessionBorder(editor: SessionBorderEditor, getName: () => string): void {
	if (editor[ATTACHED]) return;
	editor[ATTACHED] = true;
	const orig = editor.renderTopBorder.bind(editor);
	editor.renderTopBorder = (width: number, hiddenLineCount: number) =>
		paintTopBorder(
			(leftWidth) => orig(leftWidth, hiddenLineCount),
			(text) => editor.borderColor(text),
			getName(),
			width,
		);
}

export default function (pi: ExtensionAPI): void {
	let requestRender: (() => void) | undefined;

	pi.on("session_start", (_event, ctx) => {
		if (ctx.mode !== "tui") return;
		const previous = ctx.ui.getEditorComponent();
		ctx.ui.setEditorComponent((tui, theme, keybindings) => {
			requestRender = () => tui.requestRender();
			const inner = previous?.(tui, theme, keybindings);
			const editor =
				inner ?? new CustomEditor(tui, theme, keybindings, { embedWorkingStatus: true });
			if (isSessionBorderEditor(editor)) attachSessionBorder(editor, () => pi.getSessionName() ?? "");
			return editor;
		});
	});

	pi.on("session_info_changed", () => requestRender?.());
	pi.on("session_shutdown", () => {
		requestRender = undefined;
	});
}
