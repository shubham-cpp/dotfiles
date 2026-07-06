---{{{ Mini starter
local starter = require "mini.starter"
local my_items = {
  starter.sections.builtin_actions(),
  starter.sections.sessions(9, true),
  { name = "Recent Files", action = ":Pick oldfiles", section = "MiniPick" },
  { name = "File Picker", action = ":Pick files", section = "MiniPick" },
  { name = "Git Files", action = ':Pick files tool="git"', section = "MiniPick" },
  { name = "Select Sessions", action = ":lua MiniSessions.select()", section = "MiniPick" },
  starter.sections.recent_files(5, true),
}
starter.setup({
  autoopen = true,
  evaluate_single = true,
  items = my_items,
  header = nil,
  footer = nil,
  content_hooks = {
    starter.gen_hook.adding_bullet(),
    starter.gen_hook.indexing("all", { "Builtin actions", "MiniPick" }),
    starter.gen_hook.padding(5, 2),
    starter.gen_hook.aligning("left", "top"),
  },
  query_updaters = [[abcdefghijklmnopqrstuvwxyz0123456789_-.]],
})
---}}}
