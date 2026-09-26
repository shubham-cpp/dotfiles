import { Plugin } from "@opencode/plugin"
import { IdleInhibitor } from "./inhibitor"
import { createTracker, type SessionEventLike } from "./tracker"

const MAX_STREAM_RESTARTS = 3

function abortableDelay(milliseconds: number, signal: AbortSignal): Promise<void> {
  if (signal.aborted) return Promise.resolve()
  return new Promise((resolve) => {
    const finish = () => {
      clearTimeout(timer)
      signal.removeEventListener("abort", finish)
      resolve()
    }
    const timer = setTimeout(finish, milliseconds)
    signal.addEventListener("abort", finish, { once: true })
  })
}

export default Plugin.define({
  id: "idle-inhibit",
  async setup(ctx) {
    const label = ctx.location.directory
    const warn = (message: string) => console.warn(`[idle-inhibit] ${message} (${label})`)
    const inhibitor = new IdleInhibitor({ warn })
    const tracker = createTracker(
      {
        directory: ctx.location.directory,
        workspaceID: ctx.location.workspaceID,
      },
      inhibitor,
    )

    // These hooks recover executions whose live `session.execution.started`
    // event happened before this plugin instance loaded, such as restart recovery.
    await ctx.session.hook("prompt", (event) => {
      tracker.claim(event.sessionID)
    })
    await ctx.session.hook("context", (event) => tracker.markActive(event.sessionID))
    await ctx.session.hook("compaction", (event) => tracker.markActive(event.sessionID))

    const controller = new AbortController()
    const streamTask = (async () => {
      let restart = 0
      while (!controller.signal.aborted) {
        try {
          for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
            await tracker.handle(event as SessionEventLike)
          }
          if (controller.signal.aborted) return
          throw new Error("event stream closed")
        } catch (error) {
          if (controller.signal.aborted) return
          await tracker.reset()
          restart += 1
          const detail = error instanceof Error ? error.message : String(error)
          if (restart > MAX_STREAM_RESTARTS) {
            warn(`event stream unavailable after ${MAX_STREAM_RESTARTS} retries: ${detail}`)
            return
          }
          warn(`event stream interrupted; retrying (${restart}/${MAX_STREAM_RESTARTS}): ${detail}`)
          await abortableDelay(250 * 2 ** (restart - 1), controller.signal)
        }
      }
    })()

    return async () => {
      controller.abort()
      await streamTask
      await tracker.dispose()
    }
  },
})
