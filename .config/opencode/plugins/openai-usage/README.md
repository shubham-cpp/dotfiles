# Usage sidebar

Local OpenCode V2 plugin. The server resolves active provider credentials and sends only quota results to the TUI through RPC. The sidebar shows one heading, `OpenAI Usage` or `Grok Usage`, according to the active session's model family. Other families have no quota section. Quotas describe the directly connected account, even when a model family is selected through a third-party router.

## Sidebar placement

The plugin prepends Context and Usage to `sidebar.content`. `cli.json` disables the built-in `opencode.sidebar.context` contribution to avoid displaying Context twice. Other sidebar contributions, including MCP, keep their own rendering.

OpenCode 2.0.5 exposes a whole-sidebar slot but no slot between its built-in Context and MCP contributions. The local Context calculation follows the [2.0.5 session utility](https://github.com/anomalyco/opencode/blob/v2.0.5/packages/tui/src/util/session.ts) and [Context component](https://github.com/anomalyco/opencode/blob/v2.0.5/packages/tui/src/feature-plugins/sidebar/context.tsx).

## Cached input percentage

`% cached` describes the latest model call with token usage after the most recent completed compaction and before an undo boundary:

```text
100 × cache.read / (input + cache.read + cache.write)
```

OpenCode normalizes uncached input, cache reads, and cache writes into separate counts. Output and reasoning tokens do not enter this percentage. Cache writes are input but are not hits. This is a token-weighted percentage for one call, not a session-wide average or the percentage of requests that hit a cache. When input usage is unavailable, the line is omitted.

## Quota refresh

- Fetch when the TUI plugin loads.
- Refresh every three minutes.
- On `session.execution.succeeded`, refresh if the previous attempt was at least 30 seconds ago.
- Coalesce overlapping refreshes within this TUI instance.
- Cancel the request, timer, and event subscription on unload.

Quota results live in TUI memory. There is no disk cache, server TTL, or hard expiry. A transport failure can leave the previous snapshot visible with a refresh-failure message. A provider reporting unavailable replaces that provider's displayed values with an unavailable state. Each TUI instance polls independently.

The quota snapshot cache is unrelated to model prompt caching. `% cached` comes from provider token accounting, not from these polling results.

## Grok subscription support

Grok uses the active `xai` OAuth connection, selected through `/connect` → xAI → SuperGrok Subscription. The server fetches the CLI proxy's `/user` and `/billing?format=credits` endpoints with a combined 15-second timeout. The display follows the returned weekly/monthly period and supports the legacy monthly-credit format. Shared allowance covers Grok products; it is not a separate Code/Build-only budget. Extra purchased credits are not included in the remaining allowance percentage.

This endpoint comes from first-party Grok Build source, rather than a published third-party billing contract. Details and citations are in [`docs/grok-usage-research.md`](../../docs/grok-usage-research.md).

Usage rows use the same `text.muted` token as Context and the MCP "Connected" status. Only the remaining percentage below 20% uses `text.feedback.error.base`; labels and suffixes stay muted.

## Tests

```sh
bun test plugins/openai-usage
```

See [V2 CLI plugin documentation](https://opencode.ai/v2/docs/build/plugins/cli) for slot, session data, and cleanup APIs.
