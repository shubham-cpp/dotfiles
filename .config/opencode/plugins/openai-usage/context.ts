import type { ModelInfo, SessionMessageInfo } from "@opencode/client"

// Match the V2 sidebar's compaction/revert boundaries and normalized token counts.
export function contextStats(
  messages: readonly SessionMessageInfo[],
  models: readonly ModelInfo[] | undefined,
  boundary?: string,
) {
  const boundaryIndex = boundary ? messages.findIndex(message => message.id === boundary) : -1
  if (boundary && boundaryIndex === -1) return undefined
  const end = boundaryIndex === -1 ? messages.length : boundaryIndex
  const compaction = messages.findLastIndex((message, index) =>
    index < end && message.type === "compaction" && message.status === "completed",
  )
  const last = messages.findLast((message, index) =>
    index > compaction && index < end && message.type === "assistant" && message.tokens !== undefined,
  )
  if (last?.type !== "assistant" || !last.tokens) return undefined
  const usage = last.tokens
  const input = usage.input + usage.cache.read + usage.cache.write
  const tokens = input + usage.output + usage.reasoning
  if (tokens <= 0) return undefined
  const model = models?.find(model => model.providerID === last.model.providerID && model.id === last.model.id)
  return {
    tokens,
    percent: model?.limit.context ? Math.round(tokens / model.limit.context * 100) : undefined,
    cached: input > 0 ? Math.round(usage.cache.read / input * 100) : undefined,
  }
}
