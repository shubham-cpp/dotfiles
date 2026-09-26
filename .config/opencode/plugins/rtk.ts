import { Plugin } from "@opencode/plugin"
import { execFile } from "node:child_process"
import { promisify } from "node:util"

const exec = promisify(execFile)

// RTK OpenCode plugin — rewrites commands to use rtk for token savings.
// Requires: rtk >= 0.23.0 in PATH.
//
// This is a thin delegating plugin: all rewrite logic lives in `rtk rewrite`,
// which is the single source of truth (src/discover/registry.rs).
// To add or change rewrite rules, edit the Rust registry — not this file.

export default Plugin.define({ id: "rtk", async setup(ctx) {
  try {
    await exec("rtk", ["--version"])
  } catch {
    console.warn("[rtk] rtk binary not found in PATH — plugin disabled")
    return
  }

  await ctx.tool.hook("execute.before", async (input) => {
      const tool = String(input?.tool ?? "").toLowerCase()
      if (tool !== "bash" && tool !== "shell") return
      const args = input.input
      if (!args || typeof args !== "object") return

      const command = (args as Record<string, unknown>).command
      if (typeof command !== "string" || !command) return

      try {
        // RTK releases can emit a valid rewrite with a nonzero status.
        // Match the original plugin's nothrow behavior, but reject timeouts.
        const result = await exec("rtk", ["rewrite", command], { timeout: 5000 }).catch((error) => {
          if (error.killed || typeof error.stdout !== "string") throw error
          return { stdout: error.stdout }
        })
        const rewritten = String(result.stdout).trim()
        if (rewritten && rewritten !== command) {
          ;(args as Record<string, unknown>).command = rewritten
        }
      } catch {
        // rtk rewrite failed — pass through unchanged
      }
    })
}})
