import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

import { HistoryOverlay, shortSeed, type HistoryTheme } from "./overlay.ts";
import { HistoryStore } from "./store.ts";
import { extractText, timestampMs } from "./text.ts";

export default function (pi: ExtensionAPI) {
	const store = HistoryStore.open();
	let backfill: Promise<void> | undefined;

	pi.on("session_start", (_event, ctx) => {
		store.ingestMany(collectBranch(ctx));
		if (!store.isBackfilled()) {
			backfill = store.ensureBackfill(ctx.cwd);
		}
	});

	pi.on("input", (event, ctx) => {
		if (event.source === "extension") return;
		const text = event.text?.trim() ?? "";
		if (!text) return;
		store.add(text, ctx.cwd);
	});

	const open = async (ctx: ExtensionContext) => {
		if (!ctx.hasUI || ctx.mode !== "tui") {
			ctx.ui.notify("History search needs the TUI", "error");
			return;
		}

		store.ingestMany(collectBranch(ctx));
		if (backfill) await backfill;
		else await store.ensureBackfill(ctx.cwd);

		const selected = await ctx.ui.custom<string | null>(
			(tui, theme, keybindings, done) =>
				new HistoryOverlay({
					tui,
					theme: theme as HistoryTheme,
					keybindings,
					store,
					cwd: ctx.cwd,
					seed: shortSeed(ctx.ui.getEditorText()),
					done,
				}),
			{
				overlay: true,
				overlayOptions: {
					width: "70%",
					minWidth: 56,
					maxHeight: "70%",
					anchor: "center",
					margin: 1,
				},
			},
		);

		if (selected) ctx.ui.setEditorText(selected);
	};

	pi.registerShortcut("ctrl+r", {
		description: "Search prompt history",
		handler: open,
	});

	pi.registerCommand("history", {
		description: "Search prompt history",
		handler: async (_args, ctx) => open(ctx),
	});
}

function collectBranch(ctx: ExtensionContext): Array<{ prompt: string; cwd?: string; createdAt: number }> {
	const items: Array<{ prompt: string; cwd?: string; createdAt: number }> = [];
	try {
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type !== "message") continue;
			const message = entry.message as {
				role?: unknown;
				content?: unknown;
				timestamp?: unknown;
			};
			if (message.role !== "user") continue;
			const text = extractText(message.content);
			if (!text) continue;
			items.push({
				prompt: text,
				cwd: ctx.cwd,
				createdAt: timestampMs(
					(entry as { timestamp?: unknown }).timestamp,
					message.timestamp,
				),
			});
		}
	} catch {
		// Session may be ephemeral or mid-reload.
	}
	return items;
}
