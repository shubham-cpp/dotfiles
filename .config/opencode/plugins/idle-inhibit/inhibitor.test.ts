import assert from "node:assert/strict"
import { EventEmitter } from "node:events"
import { describe, test } from "node:test"
import {
  IdleInhibitor,
  parentBoundScript,
  resolveInhibitorSpec,
  type ChildHandle,
} from "./inhibitor.ts"

class FakeChild extends EventEmitter implements ChildHandle {
  exitCode: number | null = null
  readonly signals: Array<NodeJS.Signals | number | undefined> = []
  private readonly exitOnKill: boolean

  constructor(exitOnKill = true) {
    super()
    this.exitOnKill = exitOnKill
  }

  spawned(): void {
    this.emit("spawn")
  }

  crash(code = 1): void {
    this.exitCode = code
    this.emit("exit", code, null)
  }

  kill(signal?: NodeJS.Signals | number): boolean {
    this.signals.push(signal)
    if (this.exitCode !== null) return false
    if (this.exitOnKill) {
      this.exitCode = 0
      queueMicrotask(() => this.emit("exit", 0, signal))
    }
    return true
  }
}

function successfulSpawner(children: FakeChild[]) {
  return () => {
    const child = new FakeChild()
    children.push(child)
    queueMicrotask(() => child.spawned())
    return child
  }
}

describe("resolveInhibitorSpec", () => {
  test("uses the intended Linux command", () => {
    assert.deepEqual(resolveInhibitorSpec("linux", () => true), {
      command: "systemd-inhibit",
      args: [
        "--what=idle:sleep",
        "--who=opencode-idle-inhibit",
        "--why=OpenCode agent running",
        "--mode=block",
        "sleep",
        "infinity",
      ],
    })
  })

  test("uses the intended macOS command", () => {
    assert.deepEqual(resolveInhibitorSpec("darwin", () => true), {
      command: "caffeinate",
      args: ["-dimsu"],
    })
  })

  test("is unavailable on unsupported platforms or without the binary", () => {
    assert.equal(resolveInhibitorSpec("win32", () => true), undefined)
    assert.equal(resolveInhibitorSpec("linux", () => false), undefined)
  })
})

describe("parentBoundScript", () => {
  test("watches both the OpenCode server and wrapper process", () => {
    const script = parentBoundScript()
    assert.match(script, /kill -0 "\$parent"/)
    assert.match(script, /kill -0 "\$wrapper"/)
    assert.match(script, /kill "\$child"/)
  })
})

describe("IdleInhibitor", () => {
  test("shares one child across overlapping sessions", async () => {
    const children: FakeChild[] = []
    const inhibitor = new IdleInhibitor({
      platform: "linux",
      commandExists: () => true,
      spawn: successfulSpawner(children),
    })

    await inhibitor.hold("one")
    await inhibitor.hold("two")
    assert.equal(children.length, 1)
    assert.equal(inhibitor.hasLiveChild(), true)

    await inhibitor.release("one")
    assert.equal(children[0].signals.length, 0)
    await inhibitor.release("two")
    assert.ok(children[0].signals.includes("SIGTERM"))
    assert.equal(inhibitor.hasLiveChild(), false)
  })

  test("floors duplicate terminal events without touching a later run", async () => {
    const children: FakeChild[] = []
    const inhibitor = new IdleInhibitor({
      platform: "linux",
      commandExists: () => true,
      spawn: successfulSpawner(children),
    })

    await inhibitor.release("missing")
    await inhibitor.hold("one")
    await inhibitor.hold("one")
    assert.equal(children.length, 1)
    await inhibitor.release("one")
    await inhibitor.release("one")
    assert.deepEqual(children[0].signals, ["SIGTERM"])
  })

  test("kills a child when the session settles before spawn completes", async () => {
    const child = new FakeChild()
    const inhibitor = new IdleInhibitor({
      platform: "linux",
      commandExists: () => true,
      spawn: () => child,
    })

    const holding = inhibitor.hold("one")
    await new Promise<void>((resolve) => setImmediate(resolve))
    const releasing = inhibitor.release("one")
    child.spawned()
    await Promise.all([holding, releasing])
    assert.ok(child.signals.includes("SIGTERM"))
    assert.equal(inhibitor.hasLiveChild(), false)
  })

  test("restarts once after an unexpected child exit", async () => {
    const children: FakeChild[] = []
    const warnings: string[] = []
    const inhibitor = new IdleInhibitor({
      platform: "linux",
      commandExists: () => true,
      spawn: successfulSpawner(children),
      warn: (message) => warnings.push(message),
    })

    await inhibitor.hold("one")
    children[0].crash()
    await inhibitor.settled()
    assert.equal(children.length, 2)
    children[1].crash()
    await inhibitor.settled()
    assert.equal(children.length, 2)
    assert.deepEqual(warnings, ["inhibitor child exited while an agent was active"])
  })

  test("warns once and remains fail-open when no backend exists", async () => {
    const warnings: string[] = []
    const inhibitor = new IdleInhibitor({
      platform: "linux",
      commandExists: () => false,
      warn: (message) => warnings.push(message),
    })

    await inhibitor.hold("one")
    await inhibitor.hold("two")
    assert.equal(warnings.length, 1)
    assert.equal(inhibitor.hasLiveChild(), false)
    await inhibitor.reset()
    await inhibitor.hold("three")
    assert.equal(warnings.length, 2)
  })

  test("cleanup terminates the owned child", async () => {
    const children: FakeChild[] = []
    const inhibitor = new IdleInhibitor({
      platform: "darwin",
      commandExists: () => true,
      spawn: successfulSpawner(children),
    })

    await inhibitor.hold("one")
    await inhibitor.dispose()
    assert.ok(children[0].signals.includes("SIGTERM"))
    assert.equal(inhibitor.activeSessionCount(), 0)
  })
})
