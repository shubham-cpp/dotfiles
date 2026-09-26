import { expect, test } from "bun:test"
import { usageProvider, usageSnapshotSchema } from "./usage"

test("selects a single quota provider from the active model family", () => {
  expect(usageProvider({ providerID: "openai", id: "gpt-6-astra" })).toBe("OpenAI")
  expect(usageProvider({ providerID: "grok", id: "grok-code" })).toBe("Grok")
  expect(usageProvider({ providerID: "xai", id: "grok-build" })).toBe("Grok")
  expect(usageProvider({ providerID: "openrouter", id: "x-ai/grok-code-fast-1" })).toBe("Grok")
  expect(usageProvider({ providerID: "anthropic", id: "claude-sonnet-4-6" })).toBeUndefined()
  expect(usageProvider(undefined)).toBeUndefined()
})

test("RPC quota schema rejects invalid percentages", () => {
  expect(usageSnapshotSchema.safeParse({ providers: [{ provider: "Grok", status: "success", windows: [{ label: "Weekly", remaining: 101 }] }] }).success).toBe(false)
})
