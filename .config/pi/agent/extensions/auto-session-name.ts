// Auto-name pi sessions the way T3 Code titles threads.
//
// After the first turn settles on a brand-new session, a cheap model writes a
// 3-8 word title from the first user message. `/rename` regenerates from the
// current branch. `/name` still wins: an existing name is never overwritten
// except by `/rename`.
//
// Model preference, first available: gpt-5.6-luna, gpt-5.4-mini, grok-4.3.
// Falls back to the session model if none of those are authenticated.

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const TITLE_TIMEOUT_MS = 20_000;
const MAX_TITLE_CHARS = 50;
const FIRST_MESSAGE_BUDGET = 8_000;
const HISTORY_BUDGET = 8_000;
const EARLIER_CONTENT_TRUNCATION_MARKER = "[Earlier content truncated]\n\n";

const PREFERRED_MODELS = ["gpt-5.6-luna", "gpt-5.4-mini", "grok-4.3"] as const;

type ModelLike = {
	id: string;
	name?: string;
	provider: string;
};

type RegistryLike = {
	getAvailable: () => ModelLike[];
	find: (provider: string, id: string) => ModelLike | undefined;
	getApiKeyAndHeaders?: (model: ModelLike) => Promise<{
		ok?: boolean;
		apiKey?: string;
		headers?: Record<string, string>;
		env?: Record<string, string>;
	}>;
	getProviderAuth?: (provider: string) => Promise<{
		auth?: { apiKey?: string; headers?: Record<string, string>; env?: Record<string, string> };
	} | undefined>;
	streamSimple?: (
		model: ModelLike,
		context: unknown,
		options: unknown,
	) => { result: () => Promise<{ content?: unknown; errorMessage?: string }> };
	complete?: (
		model: ModelLike,
		context: unknown,
		options: unknown,
	) => Promise<{ content?: unknown; errorMessage?: string }>;
};

type ContentBlock = { type?: string; text?: string };

const INITIAL_THREAD_TITLE_PROMPT = `Generate a title that will help the user recognize this coding session weeks later.
Return JSON with exactly one key: title.

Before answering, silently reduce the request to:
- Subject: What system, feature, or problem is this really about?
- Outcome: What does the user ultimately want to understand or change?
- Incidental instructions: What only describes how the agent should do the work?

Title the subject and outcome. Discard incidental instructions.

Editorial rules:
- 3-8 words, fewer than 40 characters.
- Use a compact noun phrase or clear action phrase.
- Capture the umbrella goal when the request lists several symptoms or steps.
- Name the product change, not the mock, plan, report, branch, or PR used to produce it.
- Models, subagents, tools, output formats, and monitoring instructions do not belong in the title unless they are themselves the topic.
- For reviews, name what is being reviewed and the relevant concern. Avoid generic titles such as "Review PR 123" when linked or attached context reveals the subject.
- For research, name the question domain rather than the requested research process.
- Do not claim the work is complete.
- Do not copy and truncate the user's message.
- Avoid project names already visible in the UI, quotes, labels, filler, and trailing punctuation.
- Use attached images as primary context for UI issues.
- When a URL or attachment is the only source of the subject, use available tools to inspect it directly.
- Local git history is not evidence of what a linked PR or issue is about. Never title the thread after branch names, commit messages, or merged commits found in the checkout.
- If a linked PR or issue cannot be read, fall back to the user's stated action plus its number, such as "Take Over PR 8588". This is the one case where a PR or issue number belongs in the title.`;

function regenerateThreadTitlePrompt(previousTitle: string): string {
	return `Regenerate the title for an existing coding session so the user can recognize it weeks later.
The previous title was ${JSON.stringify(previousTitle)}.
Return JSON with exactly one key: title.

Determine the title in this order:
1. Read the USER messages first. Identify the latest explicit durable goal. The original subject remains the subject until the user clearly changes what the thread is about.
2. Use ASSISTANT messages to resolve vague links, unnamed code, and discovered product nouns. Do not promote one assistant finding into the thread subject unless the user adopts it as a new goal.
3. Compare that subject with the previous title. Preserve accurate scope words, especially when earlier content is truncated. Replace the previous title when it is generic, artifact-based, a completion update, or contradicted by the thread.
4. Title the durable subject and desired outcome, not the current workflow state.

Editorial rules:
- 3-8 words, fewer than 40 characters.
- Use a compact noun phrase or clear action phrase.
- Preserve the umbrella subject when later messages focus on one finding, provider, platform, or implementation detail.
- A thread progressing through research, planning, implementation, review, CI, merge, and monitoring has usually not changed subjects.
- Ignore deliverables and operations such as mocks, plans, HTML, branches, PRs, tests, CI, commits, merging, and monitoring unless they are the actual topic.
- Models, subagents, tools, output formats, and monitoring instructions do not belong in the title unless they are themselves the topic.
- Treat final operational follow-ups and assistant completion summaries as weak evidence of subject.
- For reviews, name the reviewed feature or system and its durable concern, not one finding from the review.
- For research, name the question domain rather than the research process.
- Do not claim the work is complete.
- Do not copy and truncate a thread message.
- Avoid project names already visible in the UI, PR numbers, quotes, labels, filler, and trailing punctuation.
- Use attached images as primary context for UI issues.
- When a URL or attachment is the only source of the subject, use available tools to inspect it directly.
- Local git history is not evidence of what a linked PR or issue is about. Never title the thread after branch names, commit messages, or merged commits found in the checkout.
- If a linked PR or issue cannot be read, fall back to the user's stated action plus its number, such as "Take Over PR 8588". This is the one case where a PR or issue number belongs in the title.
- Return a meaningfully improved title, not a cosmetic paraphrase of the previous title.`;
}

function extractText(content: unknown): string {
	if (typeof content === "string") return content;
	if (!Array.isArray(content)) return "";
	const parts: string[] = [];
	for (const part of content) {
		if (!part || typeof part !== "object") continue;
		const block = part as ContentBlock;
		if (block.type === "text" && typeof block.text === "string") parts.push(block.text);
	}
	return parts.join("\n");
}

function countUserMessages(ctx: ExtensionContext): number {
	let count = 0;
	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type === "message" && entry.message.role === "user") count++;
	}
	return count;
}

function firstUserMessage(ctx: ExtensionContext): string {
	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type !== "message" || entry.message.role !== "user") continue;
		const text = extractText(entry.message.content).trim();
		if (text) return text.slice(0, FIRST_MESSAGE_BUDGET);
	}
	return "";
}

function preserveMessageEnd(message: string): string {
	const alreadyTruncated = message.startsWith(EARLIER_CONTENT_TRUNCATION_MARKER);
	const contents = alreadyTruncated
		? message.slice(EARLIER_CONTENT_TRUNCATION_MARKER.length)
		: message;
	if (!alreadyTruncated && contents.length <= HISTORY_BUDGET) return contents;
	return `${EARLIER_CONTENT_TRUNCATION_MARKER}${contents.slice(-HISTORY_BUDGET)}`;
}

function conversationTranscript(ctx: ExtensionContext): string {
	const turns: string[] = [];
	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type !== "message") continue;
		const role = entry.message.role;
		if (role !== "user" && role !== "assistant") continue;
		const text = extractText(entry.message.content).trim();
		if (!text) continue;
		turns.push(`${role.toUpperCase()}: ${text}`);
	}
	return turns.join("\n\n");
}

function buildInitialPrompt(message: string): string {
	return `${INITIAL_THREAD_TITLE_PROMPT}\n\nUser message:\n${message}`;
}

function buildRegeneratePrompt(previousTitle: string, transcript: string): string {
	return `${regenerateThreadTitlePrompt(previousTitle)}\n\nThread contents:\n${preserveMessageEnd(transcript)}`;
}

function extractTitleJson(raw: string): string {
	const text = raw.trim().replace(/^```(?:json)?\s*/i, "").replace(/```$/i, "").trim();
	try {
		const parsed = JSON.parse(text) as { title?: unknown };
		if (typeof parsed.title === "string") return parsed.title;
	} catch {
		const match = text.match(/"title"\s*:\s*"((?:\\.|[^"\\])*)"/);
		if (match) {
			try {
				return JSON.parse(`"${match[1]}"`) as string;
			} catch {
				return match[1];
			}
		}
	}
	return text;
}

function sanitizeTitle(raw: string): string {
	const decoded = extractTitleJson(raw);
	const normalized = decoded
		.trim()
		.split(/\r?\n/g)[0]
		?.trim()
		.replace(/^['"`]+|['"`]+$/g, "")
		.trim()
		.replace(/\s+/g, " ");

	if (!normalized) return "";
	if (normalized.length <= MAX_TITLE_CHARS) return normalized;
	return `${normalized.slice(0, MAX_TITLE_CHARS - 3).trimEnd()}...`;
}

function responseText(response: { content?: unknown; errorMessage?: string } | undefined): string {
	if (!response) return "";
	if (response.errorMessage) throw new Error(response.errorMessage);
	if (!Array.isArray(response.content)) return "";
	return response.content
		.filter((part): part is { type: "text"; text: string } => {
			return Boolean(part && typeof part === "object" && (part as ContentBlock).type === "text");
		})
		.map((part) => part.text)
		.join("")
		.trim();
}

async function modelAuth(registry: RegistryLike, model: ModelLike) {
	if (typeof registry.getApiKeyAndHeaders === "function") {
		const auth = await registry.getApiKeyAndHeaders(model);
		if (auth?.ok && auth.apiKey) {
			return { apiKey: auth.apiKey, headers: auth.headers, env: auth.env };
		}
	}
	const result = await registry.getProviderAuth?.(model.provider);
	const auth = result?.auth;
	if (!auth?.apiKey && !headerHasAuth(auth?.headers)) return undefined;
	return { apiKey: auth?.apiKey, headers: auth?.headers, env: auth?.env };
}

function headerHasAuth(headers: Record<string, string> | undefined): boolean {
	if (!headers) return false;
	return Object.keys(headers).some((key) => key.toLowerCase() === "authorization");
}

function modelMatches(model: ModelLike, wanted: string): boolean {
	const id = model.id.toLowerCase();
	const name = (model.name ?? "").toLowerCase();
	const needle = wanted.toLowerCase();
	return id === needle || id.endsWith(`/${needle}`) || id.includes(needle) || name.includes(needle);
}

async function pickNamingModel(ctx: ExtensionContext): Promise<ModelLike | undefined> {
	const registry = ctx.modelRegistry as unknown as RegistryLike;
	const available = registry.getAvailable?.() ?? [];
	for (const wanted of PREFERRED_MODELS) {
		const matches = available.filter((model) => modelMatches(model, wanted));
		const ranked = [
			...matches.filter((model) => model.provider === "openai-codex" || model.provider === "xai"),
			...matches,
		];
		for (const model of ranked) {
			if (await modelAuth(registry, model)) return model;
		}
	}
	return ctx.model as ModelLike | undefined;
}

async function generateTitle(
	ctx: ExtensionContext,
	prompt: string,
	signal?: AbortSignal,
): Promise<string> {
	const registry = ctx.modelRegistry as unknown as RegistryLike;
	const model = await pickNamingModel(ctx);
	if (!model) throw new Error("no naming model available");
	const auth = await modelAuth(registry, model);
	if (!auth) throw new Error(`no credentials for ${model.provider}/${model.id}`);

	const controller = new AbortController();
	const onAbort = () => controller.abort();
	if (signal?.aborted) controller.abort();
	else signal?.addEventListener("abort", onAbort, { once: true });
	const timer = setTimeout(() => controller.abort(), TITLE_TIMEOUT_MS);

	const context = {
		systemPrompt: "Return JSON only.",
		messages: [
			{
				role: "user" as const,
				content: prompt,
				timestamp: Date.now(),
			},
		],
	};
	const options = {
		...auth,
		reasoning: "low" as const,
		signal: controller.signal,
	};

	try {
		let response: { content?: unknown; errorMessage?: string } | undefined;
		if (typeof registry.streamSimple === "function") {
			response = await registry.streamSimple(model, context, options).result();
		} else if (typeof registry.complete === "function") {
			response = await registry.complete(model, context, options);
		} else {
			throw new Error("model registry has no streamSimple/complete");
		}
		const title = sanitizeTitle(responseText(response));
		if (!title) throw new Error("empty title");
		return title;
	} finally {
		clearTimeout(timer);
		signal?.removeEventListener("abort", onAbort);
	}
}

export default function (pi: ExtensionAPI) {
	let namingAttempted = false;
	let renameInFlight = false;

	pi.on("session_start", (_event, ctx) => {
		namingAttempted = false;
		if (pi.getSessionName()) {
			namingAttempted = true;
			return;
		}
		if (countUserMessages(ctx) > 0) namingAttempted = true;
	});

	pi.on("agent_settled", async (_event, ctx) => {
		if (namingAttempted) return;
		namingAttempted = true;
		if (pi.getSessionName()) return;
		if (!ctx.sessionManager.getSessionFile()) return;
		const message = firstUserMessage(ctx);
		if (!message) return;
		try {
			pi.setSessionName(await generateTitle(ctx, buildInitialPrompt(message), ctx.signal));
		} catch {
			// Leave the session unnamed. /rename can retry.
		}
	});

	pi.registerCommand("rename", {
		description: "Regenerate the session name from the current conversation",
		handler: async (_args, ctx) => {
			if (renameInFlight) {
				if (ctx.hasUI) ctx.ui.notify("Rename already in progress", "warning");
				return;
			}
			const transcript = conversationTranscript(ctx);
			if (!transcript) {
				if (ctx.hasUI) ctx.ui.notify("Nothing to title yet", "warning");
				return;
			}
			renameInFlight = true;
			if (ctx.hasUI) ctx.ui.setStatus("auto-session-name", "renaming…");
			try {
				const title = await generateTitle(
					ctx,
					buildRegeneratePrompt(pi.getSessionName() ?? "untitled", transcript),
					ctx.signal,
				);
				pi.setSessionName(title);
				if (ctx.hasUI) ctx.ui.notify(`Session named: ${title}`, "info");
			} catch (err) {
				const detail = err instanceof Error ? err.message : String(err);
				if (ctx.hasUI) ctx.ui.notify(`Rename failed: ${detail}`, "warning");
			} finally {
				renameInFlight = false;
				if (ctx.hasUI) ctx.ui.setStatus("auto-session-name", undefined);
			}
		},
	});
}
