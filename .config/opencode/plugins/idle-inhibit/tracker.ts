import type { IdleInhibitor } from "./inhibitor"

export type LocationRef = {
  directory: string
  workspaceID?: string
}

export type SessionEventLike = {
  type: string
  location?: LocationRef
  data?: {
    sessionID?: string
    location?: LocationRef
    [key: string]: unknown
  }
}

export interface InhibitorControl {
  hold(sessionID: string): Promise<void>
  release(sessionID: string): Promise<void>
  reset(): Promise<void>
  dispose(): Promise<void>
}

const START_EVENTS = new Set([
  "session.execution.started",
  // Recovery signals for a plugin loaded after the live execution-start event.
  "session.step.started",
  "session.tool.called",
  "session.retry.scheduled",
  "session.compaction.started",
])

const END_EVENTS = new Set([
  "session.execution.succeeded",
  "session.execution.failed",
  "session.execution.interrupted",
  "session.deleted",
  "session.idle",
])

function sameLocation(left: LocationRef, right: LocationRef): boolean {
  return left.directory === right.directory && left.workspaceID === right.workspaceID
}

export class SessionActivityTracker {
  private readonly location: LocationRef
  private readonly inhibitor: InhibitorControl
  private readonly ownedSessions = new Set<string>()

  constructor(location: LocationRef, inhibitor: InhibitorControl) {
    this.location = location
    this.inhibitor = inhibitor
  }

  claim(sessionID: string): void {
    this.ownedSessions.add(sessionID)
  }

  markActive(sessionID: string): Promise<void> {
    this.claim(sessionID)
    return this.inhibitor.hold(sessionID)
  }

  handle(event: SessionEventLike): Promise<void> {
    const sessionID = event.data?.sessionID
    if (!sessionID) return Promise.resolve()
    if (event.type === "session.moved" && event.data?.location) {
      // Promise plugins receive the server event stream for every loaded
      // location. A move is therefore the explicit ownership handoff.
      if (sameLocation(this.location, event.data.location)) {
        this.claim(sessionID)
        return this.inhibitor.hold(sessionID)
      }
      if (!this.ownedSessions.delete(sessionID)) return Promise.resolve()
      return this.inhibitor.release(sessionID)
    }

    const localEnvelope = event.location !== undefined && sameLocation(this.location, event.location)
    const owned = this.ownedSessions.has(sessionID)
    if (!owned && !localEnvelope) return Promise.resolve()

    if (START_EVENTS.has(event.type)) {
      this.claim(sessionID)
      return this.inhibitor.hold(sessionID)
    }
    if (END_EVENTS.has(event.type)) {
      if (event.type === "session.deleted") this.ownedSessions.delete(sessionID)
      return this.inhibitor.release(sessionID)
    }
    return Promise.resolve()
  }

  reset(): Promise<void> {
    return this.inhibitor.reset()
  }

  dispose(): Promise<void> {
    return this.inhibitor.dispose()
  }
}

export function createTracker(location: LocationRef, inhibitor: IdleInhibitor): SessionActivityTracker {
  return new SessionActivityTracker(location, inhibitor)
}
