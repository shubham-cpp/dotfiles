export type ForkBoundary =
  | { readonly type: "before"; readonly messageID: string }
  | { readonly type: "through"; readonly messageID: string }

export type ConversationSession = {
  readonly id: string
  readonly parentID?: string
  readonly fork?: {
    readonly sessionID: string
    readonly boundary: ForkBoundary
  }
  readonly projectID: string
  readonly title?: string
  readonly time: {
    readonly created: number
    readonly updated: number
    readonly archived?: number
  }
}

export type ConversationMessage = {
  readonly id: string
  readonly type: string
  readonly time: { readonly created: number }
  readonly text?: string
  readonly content?: readonly {
    readonly type: string
    readonly text?: string
    readonly name?: string
  }[]
}

export type ConversationTranscriptMap = Readonly<Record<string, readonly ConversationMessage[]>>

export type ConversationTreeNode = {
  readonly session: ConversationSession
  readonly messages: readonly ConversationMessage[]
  readonly allMessages: readonly ConversationMessage[]
  readonly childrenBefore: readonly ConversationTreeNode[]
  readonly childrenByMessageID: ReadonlyMap<string, readonly ConversationTreeNode[]>
  readonly currentSessionID: string
}

export type SessionTreeRow = {
  readonly kind: "session"
  readonly id: string
  readonly depth: number
  readonly sessionID: string
  readonly currentSessionID: string
  readonly title: string
  readonly childCount: number
  readonly isCollapsible: boolean
  readonly isCollapsed: boolean
}

export type MessageTreeRow = {
  readonly kind: "message"
  readonly id: string
  readonly depth: number
  readonly sessionID: string
  readonly currentSessionID: string
  readonly messageID: string
  readonly messageType: "user" | "assistant"
  readonly preview: string
}

export type ConversationTreeRow = SessionTreeRow | MessageTreeRow

export type BranchAction =
  | { readonly kind: "navigate"; readonly sessionID: string }
  | {
      readonly kind: "fork"
      readonly sessionID: string
      readonly before?: string
      readonly replay?: string
    }

export function buildConversationTree(
  sessions: readonly ConversationSession[],
  transcripts: ConversationTranscriptMap,
  currentSessionID: string,
): ConversationTreeNode {
  const sessionByID = new Map(sessions.map((item) => [item.id, item]))
  const current = sessionByID.get(currentSessionID)
  if (!current) throw new Error(`Current session ${currentSessionID} was not loaded`)

  const root = findRootSession(current, sessionByID)
  const childrenByParentID = new Map<string, ConversationSession[]>()

  for (const item of sessions) {
    const parentID = getParentID(item)
    if (!parentID || !sessionByID.has(parentID)) continue
    const children = childrenByParentID.get(parentID)
    if (children) children.push(item)
    else childrenByParentID.set(parentID, [item])
  }

  for (const children of childrenByParentID.values()) {
    children.sort(compareSessions)
  }

  return projectSession(root, undefined, transcripts, childrenByParentID, currentSessionID, new Set())
}

export function getFamilySessionIDs(
  sessions: readonly ConversationSession[],
  currentSessionID: string,
): readonly string[] {
  const sessionByID = new Map(sessions.map((item) => [item.id, item]))
  const current = sessionByID.get(currentSessionID)
  if (!current) return []
  const root = findRootSession(current, sessionByID)
  const childrenByParentID = new Map<string, string[]>()

  for (const item of sessions) {
    const parentID = getParentID(item)
    if (!parentID) continue
    const children = childrenByParentID.get(parentID)
    if (children) children.push(item.id)
    else childrenByParentID.set(parentID, [item.id])
  }

  const result: string[] = []
  const pending = [root.id]
  const seen = new Set<string>()
  while (pending.length > 0) {
    const sessionID = pending.shift()
    if (!sessionID || seen.has(sessionID)) continue
    seen.add(sessionID)
    result.push(sessionID)
    pending.push(...(childrenByParentID.get(sessionID) ?? []))
  }
  return result
}

export function flattenConversationTree(
  root: ConversationTreeNode,
  collapsedSessionIDs: ReadonlySet<string> = new Set(),
): readonly ConversationTreeRow[] {
  const rows: ConversationTreeRow[] = []
  flattenNode(root, 0, rows, collapsedSessionIDs)
  return rows
}

export function planBranchAction(
  row:
    | Pick<SessionTreeRow, "kind" | "sessionID">
    | Pick<MessageTreeRow, "kind" | "sessionID" | "messageID" | "messageType">,
  transcript: readonly ConversationMessage[],
): BranchAction {
  if (row.kind === "session") return { kind: "navigate", sessionID: row.sessionID }

  const messageIndex = transcript.findIndex((message) => message.id === row.messageID)
  if (messageIndex < 0) throw new Error(`Message ${row.messageID} is unavailable`)

  if (row.messageType === "user") {
    const selected = transcript[messageIndex]
    return {
      kind: "fork",
      sessionID: row.sessionID,
      before: row.messageID,
      replay: selected?.text,
    }
  }

  const nextMessageID = transcript[messageIndex + 1]?.id
  return nextMessageID
    ? { kind: "fork", sessionID: row.sessionID, before: nextMessageID }
    : { kind: "fork", sessionID: row.sessionID }
}

export function getMessagePreview(message: ConversationMessage): string {
  if (message.type === "user") return normalizePreview(message.text ?? "(empty prompt)")

  const text = message.content?.find((part) => part.type === "text" && part.text?.trim())?.text
  if (text) return normalizePreview(text)

  const tool = message.content?.find((part) => part.type === "tool")
  if (tool) return `tool:${tool.name ?? "unknown"}`

  const reasoning = message.content?.find(
    (part) => part.type === "reasoning" && part.text?.trim(),
  )?.text
  if (reasoning) return normalizePreview(`reasoning: ${reasoning}`)

  return "(no visible content)"
}

function findRootSession(
  current: ConversationSession,
  sessionByID: ReadonlyMap<string, ConversationSession>,
): ConversationSession {
  const visited = new Set<string>()
  let candidate = current

  while (!visited.has(candidate.id)) {
    visited.add(candidate.id)
    const parentID = getParentID(candidate)
    if (!parentID) return candidate
    const parent = sessionByID.get(parentID)
    if (!parent) return candidate
    candidate = parent
  }

  return candidate
}

function projectSession(
  session: ConversationSession,
  parentMessages: readonly ConversationMessage[] | undefined,
  transcripts: ConversationTranscriptMap,
  childrenByParentID: ReadonlyMap<string, readonly ConversationSession[]>,
  currentSessionID: string,
  ancestors: ReadonlySet<string>,
): ConversationTreeNode {
  if (ancestors.has(session.id)) throw new Error(`Session hierarchy contains a cycle at ${session.id}`)

  const nextAncestors = new Set(ancestors)
  nextAncestors.add(session.id)
  const allMessages = sortMessages(transcripts[session.id] ?? [])
  const inheritedCount = parentMessages ? getCommonPrefixLength(parentMessages, allMessages) : 0
  const messages = allMessages.slice(inheritedCount).filter(isConversationMessage)
  const visibleMessageIDs = new Set(messages.map((message) => message.id))
  const childrenBefore: ConversationTreeNode[] = []
  const childrenByMessageID = new Map<string, ConversationTreeNode[]>()

  for (const child of childrenByParentID.get(session.id) ?? []) {
    const childNode = projectSession(
      child,
      allMessages,
      transcripts,
      childrenByParentID,
      currentSessionID,
      nextAncestors,
    )
    const anchorMessageID = getForkAnchorMessageID(child, allMessages)

    if (!anchorMessageID || !visibleMessageIDs.has(anchorMessageID)) {
      childrenBefore.push(childNode)
      continue
    }

    const anchored = childrenByMessageID.get(anchorMessageID)
    if (anchored) anchored.push(childNode)
    else childrenByMessageID.set(anchorMessageID, [childNode])
  }

  return {
    session,
    messages,
    allMessages,
    childrenBefore,
    childrenByMessageID,
    currentSessionID,
  }
}

function flattenNode(
  node: ConversationTreeNode,
  depth: number,
  rows: ConversationTreeRow[],
  collapsedSessionIDs: ReadonlySet<string>,
): void {
  const childCount =
    node.childrenBefore.length +
    [...node.childrenByMessageID.values()].reduce((total, children) => total + children.length, 0)

  rows.push({
    kind: "session",
    id: `session:${node.session.id}`,
    depth,
    sessionID: node.session.id,
    currentSessionID: node.currentSessionID,
    title: node.session.title?.trim() || node.session.id,
    childCount,
    isCollapsible: childCount > 0 || node.messages.length > 0,
    isCollapsed: collapsedSessionIDs.has(node.session.id),
  })

  if (collapsedSessionIDs.has(node.session.id)) return

  for (const child of node.childrenBefore) flattenNode(child, depth + 1, rows, collapsedSessionIDs)

  for (const message of node.messages) {
    const messageType = message.type as "user" | "assistant"
    rows.push({
      kind: "message",
      id: `message:${node.session.id}:${message.id}`,
      depth: depth + 1,
      sessionID: node.session.id,
      currentSessionID: node.currentSessionID,
      messageID: message.id,
      messageType,
      preview: getMessagePreview(message),
    })
    for (const child of node.childrenByMessageID.get(message.id) ?? []) {
      flattenNode(child, depth + 2, rows, collapsedSessionIDs)
    }
  }
}

function getForkAnchorMessageID(
  child: ConversationSession,
  parentMessages: readonly ConversationMessage[],
): string | undefined {
  const boundary = child.fork?.boundary
  if (!boundary) return findPreviousConversationMessage(parentMessages, parentMessages.length)?.id

  const boundaryIndex = parentMessages.findIndex((message) => message.id === boundary.messageID)
  if (boundaryIndex < 0) return undefined

  if (boundary.type === "before" && parentMessages[boundaryIndex]?.type === "user") {
    return parentMessages[boundaryIndex]?.id
  }

  const endExclusive = boundary.type === "before" ? boundaryIndex : boundaryIndex + 1
  return findPreviousConversationMessage(parentMessages, endExclusive)?.id
}

function findPreviousConversationMessage(
  messages: readonly ConversationMessage[],
  endExclusive: number,
): ConversationMessage | undefined {
  for (let index = endExclusive - 1; index >= 0; index -= 1) {
    const message = messages[index]
    if (message && isConversationMessage(message)) return message
  }
  return undefined
}

function getCommonPrefixLength(
  parent: readonly ConversationMessage[],
  child: readonly ConversationMessage[],
): number {
  const limit = Math.min(parent.length, child.length)
  let index = 0
  while (index < limit && parent[index]?.id === child[index]?.id) index += 1
  return index
}

function isConversationMessage(
  message: ConversationMessage,
): message is ConversationMessage & { readonly type: "user" | "assistant" } {
  return message.type === "user" || message.type === "assistant"
}

function getParentID(session: ConversationSession): string | undefined {
  return session.fork?.sessionID
}

function compareSessions(left: ConversationSession, right: ConversationSession): number {
  return left.time.created - right.time.created || left.id.localeCompare(right.id)
}

function sortMessages(messages: readonly ConversationMessage[]): readonly ConversationMessage[] {
  return [...messages].sort(
    (left, right) => left.time.created - right.time.created || left.id.localeCompare(right.id),
  )
}

function normalizePreview(value: string): string {
  const normalized = value.replace(/\s+/g, " ").trim()
  if (!normalized) return "(empty text)"
  return normalized.length > 120 ? `${normalized.slice(0, 117)}...` : normalized
}
