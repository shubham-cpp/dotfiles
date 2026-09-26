// Locally maintained V2 port of the codebase-memory-mcp augmentation plugin.
// OpenCode already reaches every tool over MCP; this module adds the
// context surfaces other clients get through hook configuration: graph
// lookup after grep/glob, index-coverage notes after read, session-start
// tier routing (carried on the first tool result of each session, since
// OpenCode documents no context-output lifecycle hook), and reinjection
// after compaction through the model-context hook.
import { spawn } from 'node:child_process';
import { Plugin } from '@opencode/plugin';

const BIN = '/home/shubham/.local/bin/codebase-memory-mcp';

function augment(payload: Record<string, unknown>): Promise<string> {
  return new Promise((resolve) => {
    const child = spawn(BIN, ['hook-augment'], {
      timeout: 10000,
      stdio: ['pipe', 'pipe', 'ignore'],
      env: { ...process.env, CBM_LOG_LEVEL: 'error' },
    });
    let out = '';
    child.stdout.on('data', (d) => (out += d.toString()));
    child.on('error', () => resolve(''));
    child.stdin.on('error', () => resolve(''));
    child.on('close', () => {
      try {
        const ctx = JSON.parse(out)?.hookSpecificOutput?.additionalContext;
        resolve(typeof ctx === 'string' ? ctx : '');
      } catch { resolve(''); }
    });
    child.stdin.end(JSON.stringify(payload));
  });
}

export default Plugin.define({ id: 'codebase-memory.augment', async setup(ctx) {
  const dir = ctx.location.directory;
  const seen = new Set();
  const lifecycle = () =>
    augment({ hook_event_name: 'SessionStart', cwd: dir });
  await ctx.tool.hook('execute.after', async (input) => {
      if (input.status !== 'completed') return;
      const pieces = [];
      const sid = input?.sessionID;
      if (typeof sid === 'string' && !seen.has(sid)) {
        seen.add(sid);
        pieces.push(await lifecycle());
      }
      const args = (input.input ?? {}) as Record<string, unknown>;
      const search =
        input?.tool === 'grep' ? 'Grep' : input?.tool === 'glob' ? 'Glob' : null;
      if (search) {
        pieces.push(await augment({
          hook_event_name: 'PreToolUse',
          tool_name: search,
          tool_input: args,
          cwd: dir,
        }));
      } else if (input?.tool === 'read') {
        const filePath = args.filePath ?? args.file_path ?? args.path;
        if (typeof filePath === 'string' && filePath) {
          pieces.push(await augment({
            hook_event_name: 'PostToolUse',
            tool_name: 'Read',
            tool_input: { file_path: filePath },
            cwd: dir,
          }));
        }
      }
      const extra = pieces.filter(Boolean).join('\n');
      if (extra) {
        const content = input.result.content;
        input.result = { ...input.result, content: typeof content === 'string' ? content + '\n' + extra : [...(content ?? []), { type: 'text', text: extra }] };
      }
    });
    // Reapply current graph guidance on model requests, including after compaction.
    await ctx.session.hook('context', async (event) => {
      const note = await lifecycle();
      if (note) {
        event.system.push({ type: 'text', text: note });
      }
    });
}});
