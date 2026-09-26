import {
  buildConversationTree,
  getFamilySessionIDs,
  type ConversationMessage,
  type ConversationSession,
  type ConversationTreeNode,
  type ConversationTranscriptMap,
} from "./core.ts"

export type ConversationReadClient = {
  readonly session: {
    get(input: { readonly sessionID: string }): Promise<ConversationSession>
    list(input: {
      readonly project: string
      readonly limit: number
      readonly order?: "asc" | "desc"
      readonly cursor?: string
    }): Promise<{
      readonly data: readonly ConversationSession[]
      readonly cursor: { readonly next?: string | null }
    }>
  }
  readonly message: {
    list(input: {
      readonly sessionID: string
      readonly limit: number
      readonly order?: "asc" | "desc"
      readonly cursor?: string
    }): Promise<{
      readonly data: readonly ConversationMessage[]
      readonly cursor: { readonly next?: string | null }
    }>
  }
}

export type LoadedConversationTree = {
  readonly tree: ConversationTreeNode
  readonly sessions: readonly ConversationSession[]
  readonly transcripts: ConversationTranscriptMap
}

const PAGE_SIZE = 100

export async function loadConversationTree(
  client: ConversationReadClient,
  currentSessionID: string,
): Promise<LoadedConversationTree> {
  const current = await client.session.get({ sessionID: currentSessionID })
  const projectSessions = await loadProjectSessions(client, current.projectID)
  const sessions = projectSessions.some((session) => session.id === current.id)
    ? projectSessions
    : [...projectSessions, current]
  const familySessionIDs = getFamilySessionIDs(sessions, currentSessionID)
  const familySessionIDSet = new Set(familySessionIDs)
  const familySessions = sessions.filter((session) => familySessionIDSet.has(session.id))
  const transcriptEntries = await Promise.all(
    familySessionIDs.map(async (sessionID) => {
      const messages = await loadSessionMessages(client, sessionID)
      return [sessionID, messages] as const
    }),
  )
  const transcripts = Object.fromEntries(transcriptEntries)

  return {
    tree: buildConversationTree(familySessions, transcripts, currentSessionID),
    sessions: familySessions,
    transcripts,
  }
}

export async function loadProjectSessions(
  client: ConversationReadClient,
  projectID: string,
): Promise<readonly ConversationSession[]> {
  const sessions = new Map<string, ConversationSession>()
  const seenCursors = new Set<string>()
  let cursor: string | undefined

  while (true) {
    const page = await client.session.list({
      project: projectID,
      limit: PAGE_SIZE,
      ...(cursor ? { cursor } : { order: "asc" as const }),
    })
    for (const session of page.data) sessions.set(session.id, session)

    const next = page.cursor.next ?? undefined
    if (!next) return [...sessions.values()]
    if (seenCursors.has(next)) throw new Error("Session pagination returned a repeated cursor")
    seenCursors.add(next)
    cursor = next
  }
}

export async function loadSessionMessages(
  client: ConversationReadClient,
  sessionID: string,
): Promise<readonly ConversationMessage[]> {
  const messages = new Map<string, ConversationMessage>()
  const seenCursors = new Set<string>()
  let cursor: string | undefined

  while (true) {
    const page = await client.message.list({
      sessionID,
      limit: PAGE_SIZE,
      ...(cursor ? { cursor } : { order: "asc" as const }),
    })
    for (const message of page.data) messages.set(message.id, message)

    const next = page.cursor.next ?? undefined
    if (!next) {
      return [...messages.values()].sort(
        (left, right) => left.time.created - right.time.created || left.id.localeCompare(right.id),
      )
    }
    if (seenCursors.has(next)) {
      throw new Error(`Message pagination returned a repeated cursor for ${sessionID}`)
    }
    seenCursors.add(next)
    cursor = next
  }
}
