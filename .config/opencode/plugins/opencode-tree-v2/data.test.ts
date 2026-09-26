import assert from "node:assert/strict"
import { test } from "node:test"
import { loadConversationTree, loadProjectSessions, loadSessionMessages } from "./data.ts"
import type { ConversationMessage, ConversationSession } from "./core.ts"

const root: ConversationSession = {
  id: "ses_root",
  projectID: "project",
  time: { created: 1, updated: 1 },
}

const child: ConversationSession = {
  id: "ses_child",
  parentID: "ses_root",
  fork: { sessionID: "ses_root", boundary: { type: "before", messageID: "msg_2" } },
  projectID: "project",
  time: { created: 2, updated: 2 },
}

const messages: ConversationMessage[] = [
  { id: "msg_1", type: "user", text: "hello", time: { created: 1 } },
  { id: "msg_2", type: "assistant", content: [{ type: "text", text: "hi" }], time: { created: 2 } },
]

test("loadProjectSessions follows every cursor", async () => {
  const calls: unknown[] = []
  const client = {
    session: {
      get: async () => root,
      list: async (input: any) => {
        calls.push(input)
        return input.cursor
          ? { data: [child], cursor: {} }
          : { data: [root], cursor: { next: "page-2" } }
      },
    },
    message: { list: async () => ({ data: [], cursor: {} }) },
  }

  assert.deepEqual((await loadProjectSessions(client, "project")).map((item) => item.id), [
    "ses_root",
    "ses_child",
  ])
  assert.deepEqual(calls, [
    { project: "project", limit: 100, order: "asc" },
    { project: "project", limit: 100, cursor: "page-2" },
  ])
})

test("loadSessionMessages sorts and de-duplicates paginated messages", async () => {
  const client = {
    session: { get: async () => root, list: async () => ({ data: [], cursor: {} }) },
    message: {
      list: async (input: any) => input.cursor
        ? { data: [messages[0]], cursor: {} }
        : { data: [messages[1]], cursor: { next: "older" } },
    },
  }

  assert.deepEqual((await loadSessionMessages(client, "ses_root")).map((item) => item.id), [
    "msg_1",
    "msg_2",
  ])
})

test("loadConversationTree loads transcripts only for the selected family", async () => {
  const requested: string[] = []
  const unrelated = { ...root, id: "ses_other" }
  const client = {
    session: {
      get: async () => child,
      list: async () => ({ data: [root, child, unrelated], cursor: {} }),
    },
    message: {
      list: async ({ sessionID }: { sessionID: string }) => {
        requested.push(sessionID)
        return { data: messages, cursor: {} }
      },
    },
  }

  const loaded = await loadConversationTree(client, "ses_child")
  assert.equal(loaded.tree.session.id, "ses_root")
  assert.deepEqual(requested.sort(), ["ses_child", "ses_root"])
})
