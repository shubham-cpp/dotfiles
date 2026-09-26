import { spawn as nodeSpawn } from "node:child_process"
import { accessSync, constants } from "node:fs"
import path from "node:path"
import process from "node:process"

const WHO = "opencode-idle-inhibit"
const WHY = "OpenCode agent running"
const WATCH_NAME = "opencode-idle-inhibit-watch"
const STOP_KILL_MS = 1_000
const FORCE_SETTLE_MS = 250

export type InhibitorSpec = {
  command: string
  args: string[]
}

export interface ChildHandle {
  exitCode: number | null
  once(event: string, listener: (...args: any[]) => void): this
  off(event: string, listener: (...args: any[]) => void): this
  kill(signal?: NodeJS.Signals | number): boolean
}

type SpawnChild = (command: string, args: string[], options: { stdio: "ignore" }) => ChildHandle

export type InhibitorOptions = {
  platform?: NodeJS.Platform
  pid?: number
  pathValue?: string
  commandExists?: (command: string, pathValue: string) => boolean
  spawn?: SpawnChild
  warn?: (message: string) => void
}

function executableOnPath(command: string, pathValue: string): boolean {
  for (const directory of pathValue.split(path.delimiter)) {
    if (!directory) continue
    try {
      accessSync(path.join(directory, command), constants.X_OK)
      return true
    } catch {
      // Keep searching PATH.
    }
  }
  return false
}

export function resolveInhibitorSpec(
  platform: NodeJS.Platform,
  commandExists: (command: string) => boolean,
): InhibitorSpec | undefined {
  if (platform === "darwin") {
    if (!commandExists("caffeinate")) return undefined
    return { command: "caffeinate", args: ["-dimsu"] }
  }
  if (platform === "linux") {
    if (!commandExists("systemd-inhibit")) return undefined
    return {
      command: "systemd-inhibit",
      args: [
        "--what=idle:sleep",
        `--who=${WHO}`,
        `--why=${WHY}`,
        "--mode=block",
        "sleep",
        "infinity",
      ],
    }
  }
  return undefined
}

export function parentBoundScript(): string {
  return [
    "parent=$1; shift",
    "wrapper=$$",
    '"$@" & child=$!',
    '( while kill -0 "$parent" 2>/dev/null && kill -0 "$wrapper" 2>/dev/null; do sleep 5; done; kill "$child" 2>/dev/null ) & watcher=$!',
    'cleanup() { kill "$watcher" 2>/dev/null; kill "$child" 2>/dev/null; wait "$child" 2>/dev/null; }',
    "trap 'cleanup; exit 0' INT TERM HUP EXIT",
    'wait "$child"; status=$?',
    'kill "$watcher" 2>/dev/null',
    "trap - EXIT",
    'exit "$status"',
  ].join("; ")
}

export class IdleInhibitor {
  private readonly platform: NodeJS.Platform
  private readonly pid: number
  private readonly pathValue: string
  private readonly commandExists: (command: string, pathValue: string) => boolean
  private readonly spawn: SpawnChild
  private readonly warn: (message: string) => void
  private readonly sessions = new Set<string>()

  private queue: Promise<void> = Promise.resolve()
  private child: ChildHandle | undefined
  private specResolved = false
  private spec: InhibitorSpec | undefined
  private disposed = false
  private warnedUnavailable = false
  private warnedFailure = false
  private retryUsed = false

  constructor(options: InhibitorOptions = {}) {
    this.platform = options.platform ?? process.platform
    this.pid = options.pid ?? process.pid
    this.pathValue = options.pathValue ?? process.env.PATH ?? ""
    this.commandExists = options.commandExists ?? executableOnPath
    this.spawn =
      options.spawn ??
      ((command, args, spawnOptions) => nodeSpawn(command, args, spawnOptions) as unknown as ChildHandle)
    this.warn = options.warn ?? ((message) => console.warn(`[idle-inhibit] ${message}`))
  }

  hold(sessionID: string): Promise<void> {
    if (this.disposed || this.sessions.has(sessionID)) return this.settled()
    const wasIdle = this.sessions.size === 0
    this.sessions.add(sessionID)
    if (wasIdle) {
      this.warnedUnavailable = false
      this.warnedFailure = false
      this.retryUsed = false
    }
    return this.enqueue(() => this.reconcile())
  }

  release(sessionID: string): Promise<void> {
    if (!this.sessions.delete(sessionID)) return this.settled()
    return this.enqueue(() => this.reconcile())
  }

  reset(): Promise<void> {
    this.sessions.clear()
    return this.enqueue(() => this.reconcile())
  }

  async dispose(): Promise<void> {
    if (this.disposed) return this.settled()
    this.disposed = true
    this.sessions.clear()
    await this.enqueue(() => this.reconcile())
    await this.settled()
  }

  activeSessionCount(): number {
    return this.sessions.size
  }

  hasLiveChild(): boolean {
    return this.child !== undefined && this.child.exitCode === null
  }

  async settled(): Promise<void> {
    let current: Promise<void>
    do {
      current = this.queue
      await current
    } while (current !== this.queue)
  }

  private enqueue(work: () => Promise<void>): Promise<void> {
    this.queue = this.queue.then(work, work).catch((error) => {
      const detail = error instanceof Error ? error.message : String(error)
      this.warn(`internal operation failed: ${detail}`)
    })
    return this.queue
  }

  private resolveSpec(): InhibitorSpec | undefined {
    if (!this.specResolved) {
      this.specResolved = true
      this.spec = resolveInhibitorSpec(this.platform, (command) => this.commandExists(command, this.pathValue))
    }
    return this.spec
  }

  private async reconcile(): Promise<void> {
    if (this.disposed || this.sessions.size === 0) {
      const child = this.child
      this.child = undefined
      await this.stopChild(child)
      return
    }
    if (this.child?.exitCode === null) return
    this.child = undefined
    await this.startChild()
  }

  private async startChild(): Promise<void> {
    if (this.disposed || this.sessions.size === 0 || this.child?.exitCode === null) return
    const spec = this.resolveSpec()
    if (!spec) {
      this.warnUnavailableOnce()
      return
    }

    let child: ChildHandle
    try {
      child = this.spawn(
        "sh",
        ["-c", parentBoundScript(), WATCH_NAME, String(this.pid), spec.command, ...spec.args],
        { stdio: "ignore" },
      )
    } catch (error) {
      await this.handleStartFailure(error)
      return
    }

    this.child = child
    this.attachExitHandler(child)
    const result = await this.waitForSpawn(child)

    if (result.error) {
      if (this.child === child) this.child = undefined
      await this.stopChild(child)
      await this.handleStartFailure(result.error)
      return
    }

    if (this.disposed || this.sessions.size === 0 || this.child !== child) {
      if (this.child === child) this.child = undefined
      await this.stopChild(child)
      return
    }

    if (child.exitCode !== null) {
      if (this.child === child) this.child = undefined
      await this.retryAfterExit()
    }
  }

  private attachExitHandler(child: ChildHandle): void {
    child.once("exit", () => {
      void this.enqueue(async () => {
        if (this.child !== child) return
        this.child = undefined
        if (this.disposed || this.sessions.size === 0) return
        await this.retryAfterExit()
      })
    })
  }

  private waitForSpawn(child: ChildHandle): Promise<{ error?: Error }> {
    return new Promise((resolve) => {
      const onError = (error: Error) => {
        child.off("spawn", onSpawn)
        resolve({ error })
      }
      const onSpawn = () => {
        child.off("error", onError)
        resolve({})
      }
      child.once("error", onError)
      child.once("spawn", onSpawn)
    })
  }

  private async handleStartFailure(error: unknown): Promise<void> {
    const detail = error instanceof Error ? error.message : String(error)
    this.warnFailureOnce(`spawn failed: ${detail}`)
    if (this.disposed || this.sessions.size === 0 || this.retryUsed) return
    this.retryUsed = true
    await this.startChild()
  }

  private async retryAfterExit(): Promise<void> {
    if (this.disposed || this.sessions.size === 0) return
    if (this.retryUsed) {
      this.warnFailureOnce("inhibitor child exited while an agent was active")
      return
    }
    this.retryUsed = true
    await this.startChild()
  }

  private warnUnavailableOnce(): void {
    if (this.warnedUnavailable) return
    this.warnedUnavailable = true
    this.warn("no supported inhibitor found; install systemd-inhibit on Linux or use macOS caffeinate")
  }

  private warnFailureOnce(message: string): void {
    if (this.warnedFailure) return
    this.warnedFailure = true
    this.warn(message)
  }

  private async stopChild(child: ChildHandle | undefined): Promise<void> {
    if (!child || child.exitCode !== null) return
    await new Promise<void>((resolve) => {
      let done = false
      let forceTimer: ReturnType<typeof setTimeout> | undefined
      let settleTimer: ReturnType<typeof setTimeout> | undefined
      const finish = () => {
        if (done) return
        done = true
        if (forceTimer) clearTimeout(forceTimer)
        if (settleTimer) clearTimeout(settleTimer)
        child.off("exit", finish)
        child.off("close", finish)
        child.off("error", finish)
        resolve()
      }
      child.once("exit", finish)
      child.once("close", finish)
      child.once("error", finish)
      forceTimer = setTimeout(() => {
        try {
          child.kill("SIGKILL")
        } catch {
          // The child is already gone.
        }
        settleTimer = setTimeout(finish, FORCE_SETTLE_MS)
      }, STOP_KILL_MS)
      try {
        if (!child.kill("SIGTERM")) finish()
      } catch {
        finish()
      }
    })
  }
}
