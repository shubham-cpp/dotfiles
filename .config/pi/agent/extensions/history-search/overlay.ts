import {
	Input,
	Key,
	matchesKey,
	truncateToWidth,
	visibleWidth,
	type Component,
	type Focusable,
	type TUI,
} from "@earendil-works/pi-tui";

import type { HistoryEntry, HistoryScope, HistoryStore } from "./store.ts";
import { highlightTokens, previewText, queryTokens, relativeTime } from "./text.ts";

const HISTORY_ICON = "\u{f02da}"; // nf-md-history
const SELECTED_ICON = "\u{f0142}"; // nf-md-chevron-right
const MAX_VISIBLE = 10;
const SEED_MAX_CHARS = 80;

export type HistoryTheme = {
	fg: (token: string, text: string) => string;
	bg: (token: string, text: string) => string;
	bold: (text: string) => string;
};

type Keybindings = {
	matches: (data: string, id: string) => boolean;
};

type Done = (value: string | null) => void;

export function shortSeed(editorText: string): string {
	const text = editorText.replace(/\s+/g, " ").trim();
	if (!text || text.startsWith("/")) return "";
	if (text.length > SEED_MAX_CHARS) return "";
	if (editorText.includes("\n")) return "";
	return text;
}

export class HistoryOverlay implements Component, Focusable {
	private readonly input = new Input();
	private readonly tui: TUI;
	private readonly theme: HistoryTheme;
	private readonly keybindings: Keybindings | undefined;
	private readonly store: HistoryStore;
	private readonly cwd: string;
	private readonly done: Done;

	private _focused = false;
	private query = "";
	private scope: HistoryScope = "all";
	private results: HistoryEntry[] = [];
	private selectedIndex = 0;

	constructor(options: {
		tui: TUI;
		theme: HistoryTheme;
		keybindings?: unknown;
		store: HistoryStore;
		cwd: string;
		seed: string;
		done: Done;
	}) {
		this.tui = options.tui;
		this.theme = options.theme;
		this.keybindings = asKeybindings(options.keybindings);
		this.store = options.store;
		this.cwd = options.cwd;
		this.done = options.done;
		this.query = options.seed;
		if (this.query) this.input.setValue(this.query);

		this.input.onSubmit = () => this.accept();
		this.input.onEscape = () => this.done(null);
		this.refilter();
	}

	get focused(): boolean {
		return this._focused;
	}

	set focused(value: boolean) {
		this._focused = value;
		this.input.focused = value;
	}

	handleInput(data: string): void {
		if (this.matchBinding(data, ["tui.select.up", "selectUp"], "up")) {
			this.move(-1);
			return;
		}
		if (this.matchBinding(data, ["tui.select.down", "selectDown"], "down")) {
			this.move(1);
			return;
		}
		if (this.matchBinding(data, ["tui.select.pageUp", "selectPageUp"], "pageUp")) {
			this.move(-MAX_VISIBLE);
			return;
		}
		if (this.matchBinding(data, ["tui.select.pageDown", "selectPageDown"], "pageDown")) {
			this.move(MAX_VISIBLE);
			return;
		}
		if (matchesKey(data, Key.home)) {
			this.selectedIndex = 0;
			this.tui.requestRender();
			return;
		}
		if (matchesKey(data, Key.end)) {
			this.selectedIndex = Math.max(0, this.results.length - 1);
			this.tui.requestRender();
			return;
		}
		if (matchesKey(data, Key.tab) || matchesKey(data, "shift+tab")) {
			this.scope = this.scope === "all" ? "local" : "all";
			this.refilter();
			this.tui.requestRender();
			return;
		}
		if (matchesKey(data, Key.ctrl("g"))) {
			this.done(null);
			return;
		}

		const before = this.input.getValue();
		this.input.handleInput(data);
		const after = this.input.getValue();
		if (after !== before) {
			this.query = after;
			this.refilter();
		}
		this.tui.requestRender();
	}

	render(width: number): string[] {
		const inner = Math.max(24, width - 2);
		const t = this.theme;
		const border = (s: string) => t.fg("border", s);
		const lines: string[] = [];

		lines.push(border(`╭${"─".repeat(inner)}╮`));
		lines.push(
			boxLine(
				` ${t.fg("accent", HISTORY_ICON)} ${t.bold("History")} ${t.fg("dim", `[${this.scope === "local" ? "cwd" : "all"}]`)} ${t.fg("muted", String(this.results.length))}`,
				inner,
				border,
			),
		);

		for (const inputLine of this.input.render(inner)) {
			lines.push(boxLine(inputLine, inner, border));
		}

		lines.push(border(`├${"─".repeat(inner)}┤`));

		if (this.results.length === 0) {
			const empty = this.query.trim() ? "No matching history" : "No history yet";
			lines.push(boxLine(` ${t.fg("muted", empty)}`, inner, border));
		} else {
			const { start, end } = visibleWindow(this.selectedIndex, this.results.length, MAX_VISIBLE);
			const tokens = queryTokens(this.query);
			const chevron = `${SELECTED_ICON} `;
			const gutterWidth = visibleWidth(chevron);

			for (let i = start; i < end; i++) {
				const entry = this.results[i]!;
				const selected = i === this.selectedIndex;
				const time = relativeTime(entry.createdAt);
				const timeWidth = visibleWidth(time);
				const promptBudget = Math.max(8, inner - gutterWidth - timeWidth - 1);
				const plain = truncateToWidth(previewText(entry.prompt), promptBudget);
				const highlighted = highlightTokens(plain, tokens, (chunk) =>
					t.fg("accent", t.bold(chunk)),
				);
				const body = selected ? t.bold(highlighted) : t.fg("text", highlighted);
				const gutter = selected ? t.fg("accent", chevron) : " ".repeat(gutterWidth);
				const row = `${gutter}${padVisible(body, promptBudget)} ${t.fg("dim", time)}`;
				const padded = padVisible(row, inner);
				lines.push(
					`${border("│")}${selected ? t.bg("selectedBg", padded) : padded}${border("│")}`,
				);
			}
		}

		lines.push(border(`├${"─".repeat(inner)}┤`));
		lines.push(
			boxLine(
				t.fg("dim", "↑↓ move  enter use  tab cwd/all  esc cancel"),
				inner,
				border,
			),
		);
		lines.push(border(`╰${"─".repeat(inner)}╯`));
		return lines;
	}

	invalidate(): void {
		this.input.invalidate();
	}

	private accept(): void {
		const selected = this.results[this.selectedIndex];
		if (selected) this.done(selected.prompt);
	}

	private move(delta: number): void {
		if (this.results.length === 0) return;
		this.selectedIndex = Math.min(
			this.results.length - 1,
			Math.max(0, this.selectedIndex + delta),
		);
		this.tui.requestRender();
	}

	private refilter(): void {
		this.results = this.store.search(this.query, this.scope, this.cwd);
		this.selectedIndex = 0;
	}

	private matchBinding(data: string, ids: string[], fallback: string): boolean {
		if (this.keybindings) {
			for (const id of ids) {
				if (this.keybindings.matches(data, id)) return true;
			}
		}
		return matchesKey(data, fallback);
	}
}

function asKeybindings(value: unknown): Keybindings | undefined {
	if (value && typeof (value as Keybindings).matches === "function") {
		return value as Keybindings;
	}
	return undefined;
}

function visibleWindow(selected: number, total: number, size: number): { start: number; end: number } {
	if (total <= size) return { start: 0, end: total };
	const half = Math.floor(size / 2);
	const start = Math.max(0, Math.min(selected - half, total - size));
	return { start, end: start + size };
}

function boxLine(content: string, innerWidth: number, border: (text: string) => string): string {
	const safe = visibleWidth(content) > innerWidth ? truncateToWidth(content, innerWidth) : content;
	return `${border("│")}${padVisible(safe, innerWidth)}${border("│")}`;
}

function padVisible(content: string, width: number): string {
	const vis = visibleWidth(content);
	if (vis >= width) return truncateToWidth(content, width);
	return content + " ".repeat(width - vis);
}
