import { expect, test } from "bun:test"
import type { SessionMessageInfo } from "@opencode/client"
import { contextStats } from "./context"

function assistant(id: string, input = 20, read = 70, write = 10) {
  return { id, type: "assistant", model: { providerID: "openai", id: "test" }, tokens: {
    input, output: 30, reasoning: 5, cache: { read, write },
  } } as SessionMessageInfo
}

test("cache percentage uses normalized input including cache writes, not output", () => {
  expect(contextStats([assistant("a")], [])).toEqual({ tokens: 135, cached: 70, percent: undefined })
})

test("uses latest call, honors revert boundary, and clears at compaction", () => {
  const messages = [assistant("a"), assistant("b", 100, 0, 0)]
  expect(contextStats(messages, [])?.cached).toBe(0)
  expect(contextStats(messages, [], "b")?.cached).toBe(70)
  expect(contextStats(messages, [], "missing")).toBeUndefined()
  expect(contextStats([...messages, { id: "c", type: "compaction", status: "completed" } as SessionMessageInfo], [])).toBeUndefined()
})

test("does not invent a cache percentage for zero input", () => {
  expect(contextStats([assistant("a", 0, 0, 0)], [])?.cached).toBeUndefined()
})
