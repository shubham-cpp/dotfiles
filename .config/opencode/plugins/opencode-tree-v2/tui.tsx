/** @jsxImportSource @opentui/solid */

import { Plugin } from "@opencode/plugin/tui"
import type { Context } from "@opencode/plugin/tui/context"
import { ScrollBoxRenderable, TextAttributes } from "@opentui/core"
import {
  For,
  Show,
  createEffect,
  createMemo,
  createResource,
  createSignal,
  on,
  onCleanup,
  onMount,
} from "solid-js"
import {
  flattenConversationTree,
  planBranchAction,
  type ConversationTreeRow,
} from "./core.ts"
import {
  loadConversationTree,
  type ConversationReadClient,
} from "./data.ts"

const ROUTE_NAME = "conversation-tree"
const INPUT_MODE = "conversation-tree"

type TreeRouteProps = {
  readonly api: Context
  readonly sessionID?: string
}

export default Plugin.define({
  id: "local.conversation-tree.tui",
  setup(api) {
    const unregisterRoute = api.ui.router.register({
      name: ROUTE_NAME,
      render: ({ data }) => (
        <TreeRoute
          api={api}
          sessionID={typeof data?.sessionID === "string" ? data.sessionID : undefined}
        />
      ),
    })

    const unregisterCommands = api.ui.slot({
      append: "app",
      render: () => <TreeCommands api={api} />,
    })

    return () => {
      unregisterCommands()
      unregisterRoute()
    }
  },
})

function TreeCommands(props: { readonly api: Context }) {
  props.api.keymap.layer(() => ({
    mode: "global",
    commands: [
      {
        id: "conversation-tree.open",
        title: "Conversation tree",
        description: "Browse and fork the current session's conversation branches",
        group: "Sessions",
        palette: true,
        slash: { name: "tree" },
        suggested: () => props.api.ui.router.current().type === "session",
        enabled: () => props.api.ui.router.current().type === "session",
        run: () => {
          const route = props.api.ui.router.current()
          if (route.type !== "session") return
          props.api.ui.router.navigate({
            type: "plugin",
            name: ROUTE_NAME,
            data: { sessionID: route.sessionID },
          })
        },
      },
    ],
  }))
  return null
}

function TreeRoute(props: TreeRouteProps) {
  let scrollTimer: ReturnType<typeof setTimeout> | undefined
  const [scroll, setScroll] = createSignal<ScrollBoxRenderable>()
  const [collapsedSessionIDs, setCollapsedSessionIDs] = createSignal<ReadonlySet<string>>(new Set())
  const [selectedRowID, setSelectedRowID] = createSignal<string>()
  const [busy, setBusy] = createSignal(false)
  const resourceKey = createMemo(() => props.sessionID)
  const [loaded, { refetch }] = createResource(resourceKey, async () =>
    loadConversationTree(
      props.api.client as unknown as ConversationReadClient,
      props.sessionID!,
    ),
  )
  const rows = createMemo<readonly ConversationTreeRow[]>(() => {
    const result = loaded()
    return result ? flattenConversationTree(result.tree, collapsedSessionIDs()) : []
  })
  const selectedIndex = createMemo(() =>
    rows().findIndex((row) => row.id === selectedRowID()),
  )
  const selectedRow = createMemo(() => {
    const index = selectedIndex()
    return index >= 0 ? rows()[index] : undefined
  })

  const popMode = props.api.keymap.mode.push(INPUT_MODE)
  onCleanup(popMode)

  props.api.keymap.layer(() => ({
    mode: INPUT_MODE,
    target: () => scroll(),
    priority: 100,
    commands: [
      { bind: "escape,q", run: returnToOrigin },
      { bind: "up,k", run: () => moveSelection(-1) },
      { bind: "down,j", run: () => moveSelection(1) },
      { bind: "pageup,ctrl+b", run: () => moveSelection(-10) },
      { bind: "pagedown,ctrl+f", run: () => moveSelection(10) },
      { bind: "left,h", run: collapseSelected },
      { bind: "right,l", run: expandSelected },
      { bind: "return", enabled: () => !busy(), run: runSelectedAction },
      {
        bind: "ctrl+r",
        enabled: () => !busy(),
        run: async () => {
          await Promise.resolve(refetch())
        },
      },
    ],
  }))

  createEffect(() => {
    const nextRows = rows()
    if (nextRows.length === 0) {
      setSelectedRowID(undefined)
      return
    }
    if (nextRows.some((row) => row.id === selectedRowID())) return

    const currentSessionID = props.sessionID
    const currentRows = nextRows.filter((row) => row.sessionID === currentSessionID)
    setSelectedRowID(currentRows.at(-1)?.id ?? nextRows[0]?.id)
  })

  createEffect(
    on(selectedRowID, (rowID) => {
      if (!rowID || !scroll()) return
      if (scrollTimer) clearTimeout(scrollTimer)
      scrollTimer = setTimeout(() => scroll()?.scrollChildIntoView(rowID), 0)
    }),
  )

  createEffect(() => scroll()?.focus())

  onMount(() => {
    const stop = props.api.data.listen(({ details }) => {
      if (
        details.type === "session.forked" ||
        details.type === "session.deleted" ||
        details.type === "session.renamed"
      ) {
        void refetch()
      }
    })
    onCleanup(stop)
  })

  onCleanup(() => {
    if (scrollTimer) clearTimeout(scrollTimer)
  })

  return (
    <box flexDirection="column" width="100%" height="100%" paddingLeft={1} paddingRight={1}>
      <box flexDirection="column" paddingTop={1} paddingBottom={1}>
        <text fg={props.api.theme.text.default} attributes={TextAttributes.BOLD}>
          Conversation tree
        </text>
        <text fg={props.api.theme.text.subdued}>
          j/k move · h/l collapse · Enter open or fork · Ctrl+R refresh · Esc back
        </text>
        <text fg={props.api.theme.text.subdued}>
          Conversation branches only — every branch continues to share the current worktree.
        </text>
      </box>

      <Show when={!props.sessionID}>
        <text fg={props.api.theme.text.subdued}>Open /tree from a session.</text>
      </Show>
      <Show when={loaded.loading}>
        <text fg={props.api.theme.text.subdued}>Loading session family…</text>
      </Show>
      <Show when={loaded.error} keyed>
        {(error) => (
          <text fg={props.api.theme.text.default}>Could not load tree: {getErrorMessage(error)}</text>
        )}
      </Show>

      <Show when={!loaded.loading && !loaded.error && rows().length === 0 && props.sessionID}>
        <text fg={props.api.theme.text.subdued}>No conversation messages found.</text>
      </Show>

      <Show when={rows().length > 0}>
        <scrollbox
          ref={(value: ScrollBoxRenderable) => setScroll(value)}
          flexGrow={1}
          minHeight={0}
          width="100%"
          focusable
          scrollbarOptions={{ visible: false }}
        >
          <box flexDirection="column" width="100%">
            <For each={rows()}>
              {(row) => (
                <TreeRow
                  api={props.api}
                  row={row}
                  selected={row.id === selectedRowID()}
                />
              )}
            </For>
          </box>
        </scrollbox>
      </Show>

      <Show when={busy()}>
        <text fg={props.api.theme.text.subdued}>Creating branch…</text>
      </Show>
    </box>
  )

  function moveSelection(delta: number): void {
    const nextRows = rows()
    if (nextRows.length === 0) return
    const current = selectedIndex()
    const start = current < 0 ? (delta < 0 ? nextRows.length : -1) : current
    const next = Math.max(0, Math.min(nextRows.length - 1, start + delta))
    setSelectedRowID(nextRows[next]?.id)
  }

  function collapseSelected(): void {
    const row = getSelectedSessionRow()
    if (!row?.isCollapsible || row.isCollapsed) return
    setCollapsedSessionIDs((current) => new Set(current).add(row.sessionID))
    setSelectedRowID(row.id)
  }

  function expandSelected(): void {
    const row = getSelectedSessionRow()
    if (!row?.isCollapsed) return
    setCollapsedSessionIDs((current) => {
      const next = new Set(current)
      next.delete(row.sessionID)
      return next
    })
    setSelectedRowID(row.id)
  }

  function getSelectedSessionRow() {
    const row = selectedRow()
    if (!row) return undefined
    if (row.kind === "session") return row
    return rows().find(
      (candidate): candidate is Extract<ConversationTreeRow, { kind: "session" }> =>
        candidate.kind === "session" && candidate.sessionID === row.sessionID,
    )
  }

  function returnToOrigin(): void {
    if (props.sessionID) {
      props.api.ui.router.navigate({ type: "session", sessionID: props.sessionID })
    } else {
      props.api.ui.router.navigate({ type: "home" })
    }
  }

  async function runSelectedAction(): Promise<void> {
    const row = selectedRow()
    const result = loaded()
    if (!row || !result || busy()) return
    const transcript = result.transcripts[row.sessionID] ?? []
    const action = planBranchAction(row, transcript)

    if (action.kind === "navigate") {
      props.api.ui.router.navigate({ type: "session", sessionID: action.sessionID })
      return
    }

    let replay: string | undefined
    if (action.replay !== undefined) {
      replay = await props.api.ui.dialog.prompt({
        title: "Fork and replay prompt",
        description:
          "Edit the prompt to send in the new branch. Attachments and file state are not replayed.",
        value: action.replay,
      })
      if (replay === undefined) return
    }

    setBusy(true)
    try {
      const child = await props.api.client.session.fork(
        action.before
          ? { sessionID: action.sessionID, before: action.before }
          : { sessionID: action.sessionID },
      )
      props.api.data.session.invalidate(action.sessionID)
      props.api.data.session.invalidate(child.id)

      if (replay?.trim()) {
        await props.api.client.session.prompt({ sessionID: child.id, text: replay })
      }

      props.api.ui.router.navigate({ type: "session", sessionID: child.id })
      props.api.ui.toast.show({
        message: "Conversation branch created; files were left unchanged.",
        variant: "success",
      })
    } catch (error) {
      props.api.ui.toast.show({
        title: "Could not create branch",
        message: getErrorMessage(error),
        variant: "error",
      })
    } finally {
      setBusy(false)
    }
  }
}

function TreeRow(props: {
  readonly api: Context
  readonly row: ConversationTreeRow
  readonly selected: boolean
}) {
  const current = () => props.row.sessionID === props.row.currentSessionID
  const prefix = () => {
    const indent = "  ".repeat(props.row.depth)
    const cursor = props.selected ? "› " : "  "
    if (props.row.kind === "session") {
      const branch = props.row.isCollapsible ? (props.row.isCollapsed ? "▸ " : "▾ ") : "• "
      return `${cursor}${indent}${branch}`
    }
    return `${cursor}${indent}${props.row.messageType === "user" ? "you " : "ai  "}`
  }
  const body = () => {
    if (props.row.kind === "session") {
      const count = props.row.childCount > 0 ? ` · ${props.row.childCount} branch${props.row.childCount === 1 ? "" : "es"}` : ""
      return `${props.row.title}${current() ? " · current" : ""}${count}`
    }
    return props.row.preview
  }

  return (
    <box id={props.row.id} width="100%" flexDirection="row">
      <text
        wrapMode="none"
        fg={props.selected || current() ? props.api.theme.text.default : props.api.theme.text.subdued}
        attributes={props.selected || current() ? TextAttributes.BOLD : undefined}
      >
        {prefix()}{body()}
      </text>
    </box>
  )
}

function getErrorMessage(error: unknown): string {
  if (error instanceof Error) return error.message
  return String(error)
}
