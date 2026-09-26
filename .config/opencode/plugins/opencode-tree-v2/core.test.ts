import assert from "node:assert/strict"
import { describe, test } from "node:test"
import {
  buildConversationTree,
  flattenConversationTree,
  getFamilySessionIDs,
  getMessagePreview,
  planBranchAction,
  type ConversationMessage,
  type ConversationSession,
} from "./core.ts"

const session = (
  id: string,
  input: Partial<ConversationSession> = {},
): ConversationSession => ({
  id,
  projectID: "project",
  time: { created: Number(id.replace(/\D/g, "")) || 1, updated: 1 },
  ...input,
})

const user = (id: string, text: string): ConversationMessage => ({
  id,
  type: "user",
  text,
  time: { created: 1 },
})

const assistant = (id: string, text: string): ConversationMessage => ({
  id,
  type: "assistant",
  content: [{ type: "text", text }],
  time: { created: 1 },
})

describe("buildConversationTree", () => {
  test("uses native fork metadata and hides inherited message prefixes", () => {
    const rootMessages = [user("msg_1", "start"), assistant("msg_2", "first answer"), user("msg_3", "continue")]
    const childMessages = [user("msg_1", "start"), assistant("msg_2", "first answer"), user("msg_4", "alternative")]
    const sessions = [
      session("ses_1", { title: "Root" }),
      session("ses_2", {
        parentID: "ses_1",
        fork: { sessionID: "ses_1", boundary: { type: "before", messageID: "msg_3" } },
        title: "Alternative",
      }),
    ]

    const tree = buildConversationTree(sessions, {
      ses_1: rootMessages,
      ses_2: childMessages,
    }, "ses_2")

    assert.equal(tree.session.id, "ses_1")
    assert.deepEqual(tree.messages.map((message) => message.id), ["msg_1", "msg_2", "msg_3"])
    assert.equal(tree.childrenByMessageID.get("msg_3")?.[0]?.session.id, "ses_2")
    assert.deepEqual(tree.childrenByMessageID.get("msg_3")?.[0]?.messages.map((message) => message.id), ["msg_4"])
  })

  test("keeps unanchored children visible at their parent session", () => {
    const sessions = [
      session("ses_1"),
      session("ses_2", {
        parentID: "ses_1",
        fork: { sessionID: "ses_1", boundary: { type: "before", messageID: "msg_missing" } },
      }),
    ]

    const tree = buildConversationTree(sessions, { ses_1: [], ses_2: [] }, "ses_1")
    assert.deepEqual(tree.childrenBefore.map((child) => child.session.id), ["ses_2"])
  })
})

describe("flattenConversationTree", () => {
  test("places a child branch after its anchor message", () => {
    const sessions = [
      session("ses_1"),
      session("ses_2", {
        parentID: "ses_1",
        fork: { sessionID: "ses_1", boundary: { type: "through", messageID: "msg_2" } },
      }),
    ]
    const messages = [user("msg_1", "start"), assistant("msg_2", "answer")]
    const tree = buildConversationTree(sessions, { ses_1: messages, ses_2: messages }, "ses_1")

    assert.deepEqual(flattenConversationTree(tree).map((row) => row.id), [
      "session:ses_1",
      "message:ses_1:msg_1",
      "message:ses_1:msg_2",
      "session:ses_2",
    ])
  })

  test("collapses a session without removing its row", () => {
    const tree = buildConversationTree([session("ses_1")], {
      ses_1: [user("msg_1", "start")],
    }, "ses_1")
    assert.deepEqual(flattenConversationTree(tree, new Set(["ses_1"])).map((row) => row.id), [
      "session:ses_1",
    ])
  })
})

test("getFamilySessionIDs excludes unrelated project sessions", () => {
  const sessions = [
    session("ses_1"),
    session("ses_2", {
      parentID: "ses_1",
      fork: { sessionID: "ses_1", boundary: { type: "before", messageID: "msg_1" } },
    }),
    session("ses_3"),
  ]
  assert.deepEqual(getFamilySessionIDs(sessions, "ses_2"), ["ses_1", "ses_2"])
})

test("getFamilySessionIDs does not treat subagent parent IDs as conversation branches", () => {
  const sessions = [
    session("ses_1"),
    session("ses_subagent", { parentID: "ses_1" }),
  ]
  assert.deepEqual(getFamilySessionIDs(sessions, "ses_1"), ["ses_1"])
})

describe("planBranchAction", () => {
  const transcript = [user("msg_1", "try this"), assistant("msg_2", "done"), user("msg_3", "next")]

  test("replays a selected user prompt from a fork before it", () => {
    assert.deepEqual(planBranchAction({ kind: "message", sessionID: "ses_1", messageID: "msg_1", messageType: "user" }, transcript), {
      kind: "fork",
      sessionID: "ses_1",
      before: "msg_1",
      replay: "try this",
    })
  })

  test("retains a selected assistant response by forking before the following message", () => {
    assert.deepEqual(planBranchAction({ kind: "message", sessionID: "ses_1", messageID: "msg_2", messageType: "assistant" }, transcript), {
      kind: "fork",
      sessionID: "ses_1",
      before: "msg_3",
    })
  })

  test("forks full history when the selected assistant message is last", () => {
    assert.deepEqual(planBranchAction({ kind: "message", sessionID: "ses_1", messageID: "msg_2", messageType: "assistant" }, transcript.slice(0, 2)), {
      kind: "fork",
      sessionID: "ses_1",
    })
  })
})

test("getMessagePreview normalizes text and reports tool calls", () => {
  assert.equal(getMessagePreview(user("msg_1", "one\n  two")), "one two")
  assert.equal(getMessagePreview({
    id: "msg_2",
    type: "assistant",
    content: [{ type: "tool", name: "read" }],
    time: { created: 1 },
  }), "tool:read")
})
