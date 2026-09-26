import assert from "node:assert/strict"
import { spawnSync } from "node:child_process"
import { readFileSync, statSync } from "node:fs"
import { homedir } from "node:os"

const logPath = `${homedir()}/.local/share/opencode/log/opencode.log`
const offset = statSync(logPath).size

spawnSync(
  "timeout",
  ["4s", "script", "-q", "-c", "opencode --standalone", "/tmp/opencode/tree-tui-smoke.log"],
  { stdio: "ignore" },
)

const log = readFileSync(logPath, "utf8").slice(offset)
const failure = log
  .split("\n")
  .find(
    (line) =>
      line.includes("plugin=local.conversation-tree.tui") &&
      line.includes('error="Keymap.Provider is missing"'),
  )

assert.equal(failure, undefined, failure ?? "conversation-tree TUI plugin loaded")
console.log("conversation-tree TUI plugin loaded without Keymap.Provider errors")
