/** @jsxImportSource @opentui/solid */
import { createMemo, createSignal, For, Show } from "solid-js"
import { Plugin } from "@opencode/plugin/tui"
import type { UsageSnapshot } from "./usage"
import { usageProvider } from "./usage"
import { Usage } from "./rpc"
import { contextStats } from "./context"

const money = new Intl.NumberFormat("en-US", { style: "currency", currency: "USD" })

const REFRESH_INTERVAL_MS = 3 * 60 * 1000
const IDLE_REFRESH_MIN_AGE_MS = 30_000

export default Plugin.define({ id: "openai-usage.tui", setup(api) {
  let disposed = false
  const controller = new AbortController()
  const client = api.client.rpc(Usage)
  const [usage, setUsage] = createSignal<UsageSnapshot | null>(null)
  const [failed, setFailed] = createSignal(false)
  let refreshPromise: Promise<void> | null = null
  let lastAttemptAt = 0

  const refresh = (minimumAgeMs = 0): Promise<void> => {
    if (refreshPromise) return refreshPromise
    if (Date.now() - lastAttemptAt < minimumAgeMs) return Promise.resolve()

    lastAttemptAt = Date.now()
    refreshPromise = (async () => {
      const result = await client.get({}, { signal: controller.signal }).catch(() => null)
      if (disposed) return
      setFailed(result === null)
      if (result) setUsage(result)
    })().finally(() => {
      refreshPromise = null
    })
    return refreshPromise
  }

  void refresh()
  const timer = setInterval(() => void refresh(), REFRESH_INTERVAL_MS)
  const stop = api.data.on("session.execution.succeeded", () => void refresh(IDLE_REFRESH_MIN_AGE_MS))

  const unregister = api.ui.slot({
    prepend: "sidebar.content",
    render(props) {
        const colors = api.theme
        const session = createMemo(() => api.data.session.get(props.sessionID))
        const cost = createMemo(() => api.data.session.cost(props.sessionID))
        const providerName = createMemo(() => usageProvider(session()?.model))
        const providerUsage = createMemo(() => usage()?.providers.find(item => item.provider === providerName()))
        const stats = createMemo(() => contextStats(
          api.data.session.message.list(props.sessionID),
          api.data.location.model.list(session()?.location),
          session()?.revert?.messageID,
        ))

        return (
          <box flexDirection="column" gap={1}>
            <Show when={stats() || cost() > 0}>
              <box flexDirection="column">
                <text fg={colors.text.base}><b>Context</b></text>
                <Show when={stats()}>{value => <>
                  <text fg={colors.text.muted}>{value().tokens.toLocaleString()} tokens</text>
                  <Show when={value().percent !== undefined}>
                    <text fg={colors.text.muted}>{value().percent}% used</text>
                  </Show>
                  <Show when={value().cached !== undefined}>
                    <text fg={colors.text.muted}>{value().cached}% cached</text>
                  </Show>
                </>}</Show>
                <Show when={cost() > 0}>
                  <text fg={colors.text.muted}>{money.format(cost())} spent</text>
                </Show>
              </box>
            </Show>
            <Show when={providerName()}>{name => <box flexDirection="column">
            <text fg={colors.text.base}>
              <b>{name()} Usage</b>
            </text>
            <Show when={providerUsage()} fallback={<text fg={colors.text.muted}>{failed() || usage() ? "Usage unavailable" : "Loading..."}</text>}>
              {provider => (
                <box flexDirection="column">
                  <Show when={provider().status === "success"} fallback={
                    <text fg={colors.text.muted}>{provider().status === "auth-unavailable" ? "Not connected" : "Usage unavailable"}</text>
                  }>
                    <For each={provider().windows}>{window => (
                      <text fg={colors.text.muted}>
                        {window.label.padEnd(9)}
                        <Show when={window.remaining !== null} fallback="unavailable">
                          <span style={{ fg: window.remaining !== null && window.remaining < 20 ? colors.text.feedback.error.base : colors.text.muted }}>{window.remaining}%</span>
                          {" left"}
                        </Show>
                      </text>
                    )}</For>
                    <Show when={provider().resetsAt}>{end => <text fg={colors.text.muted}>Resets {new Date(end()).toLocaleString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" })}</text>}</Show>
                    <Show when={provider().note}>{note => <text fg={colors.text.muted}>{note()}</text>}</Show>
                  </Show>
                </box>
              )}
            </Show>
            <Show when={failed() && usage()}><text fg={colors.text.muted}>Refresh failed; showing previous values</text></Show>
            </box>}</Show>
          </box>
        )
      },
  })
  return () => { disposed = true; controller.abort(); clearInterval(timer); stop(); unregister() }
}})
