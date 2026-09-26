import assert from "node:assert/strict"
import { describe, test } from "node:test"
import { SessionActivityTracker, type InhibitorControl } from "./tracker.ts"

class FakeInhibitor implements InhibitorControl {
  readonly active = new Set<string>()
  resets = 0
  disposals = 0

  async hold(sessionID: string): Promise<void> {
    this.active.add(sessionID)
  }

  async release(sessionID: string): Promise<void> {
    this.active.delete(sessionID)
  }

  async reset(): Promise<void> {
    this.resets += 1
    this.active.clear()
  }

  async dispose(): Promise<void> {
    this.disposals += 1
    this.active.clear()
  }
}

describe("SessionActivityTracker", () => {
  test("tracks the complete execution lifecycle", async () => {
    const inhibitor = new FakeInhibitor()
    const tracker = new SessionActivityTracker({ directory: "/work" }, inhibitor)
    tracker.claim("ses_1")

    await tracker.handle({ type: "session.execution.started", data: { sessionID: "ses_1" } })
    assert.deepEqual(inhibitor.active, new Set(["ses_1"]))
    await tracker.handle({ type: "session.retry.scheduled", data: { sessionID: "ses_1" } })
    assert.deepEqual(inhibitor.active, new Set(["ses_1"]))
    await tracker.handle({ type: "session.execution.succeeded", data: { sessionID: "ses_1" } })
    assert.equal(inhibitor.active.size, 0)
  })

  test("uses activity events to recover a missed execution start", async () => {
    const inhibitor = new FakeInhibitor()
    const tracker = new SessionActivityTracker({ directory: "/work" }, inhibitor)

    await tracker.handle({
      type: "session.compaction.started",
      location: { directory: "/work" },
      data: { sessionID: "ses_1" },
    })
    assert.deepEqual(inhibitor.active, new Set(["ses_1"]))
    await tracker.handle({ type: "session.execution.interrupted", data: { sessionID: "ses_1" } })
    assert.equal(inhibitor.active.size, 0)
  })

  test("transfers a running session between locations", async () => {
    const oldInhibitor = new FakeInhibitor()
    const newInhibitor = new FakeInhibitor()
    const oldTracker = new SessionActivityTracker({ directory: "/old" }, oldInhibitor)
    const newTracker = new SessionActivityTracker({ directory: "/new" }, newInhibitor)
    await oldTracker.markActive("ses_1")

    const moved = {
      type: "session.moved",
      data: { sessionID: "ses_1", location: { directory: "/new" } },
    }
    await Promise.all([oldTracker.handle(moved), newTracker.handle(moved)])
    assert.equal(oldInhibitor.active.size, 0)
    assert.deepEqual(newInhibitor.active, new Set(["ses_1"]))
  })

  test("ignores unrelated events and events without session IDs", async () => {
    const inhibitor = new FakeInhibitor()
    const tracker = new SessionActivityTracker({ directory: "/work" }, inhibitor)

    await tracker.handle({ type: "provider.updated", data: { sessionID: "ses_1" } })
    await tracker.handle({ type: "session.execution.started", data: {} })
    assert.equal(inhibitor.active.size, 0)
  })

  test("ignores another location's execution events", async () => {
    const inhibitor = new FakeInhibitor()
    const tracker = new SessionActivityTracker({ directory: "/work" }, inhibitor)

    await tracker.handle({
      type: "session.execution.started",
      location: { directory: "/other" },
      data: { sessionID: "ses_foreign" },
    })
    await tracker.handle({ type: "session.execution.started", data: { sessionID: "ses_unknown" } })
    assert.equal(inhibitor.active.size, 0)
  })
})
