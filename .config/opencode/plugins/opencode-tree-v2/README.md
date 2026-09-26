# Local OpenCode V2 conversation tree

A local V2 port inspired by [`@ishaksebsib/opencode-tree`](https://github.com/ishaksebsib/opencode-tree).

Run `/tree` from a session to browse its native parent/child session hierarchy. Select a session to open it, a user message to fork and edit that prompt, or an assistant message to fork after that response.

This MVP branches conversation history only. It never restores snapshots or changes files, so every branch continues against the same current worktree.

## Keys

- `j` / `k` or arrows: move
- `h` / `l` or left/right: collapse/expand
- `Enter`: open or fork
- `Ctrl+R`: refresh
- `Esc` or `q`: return to the originating session

The plugin uses V2's native `session.fork`, `parentID`, and fork-boundary metadata. Its own storage does not duplicate the conversation tree.
