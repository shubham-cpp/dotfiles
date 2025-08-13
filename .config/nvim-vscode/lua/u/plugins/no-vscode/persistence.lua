local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "folke/persistence.nvim" })
  local persistence = require "persistence"
  persistence.setup()
  -- load the session for the current directory
  vim.keymap.set("n", "<leader>ql", persistence.load, { desc = "Load current directory" })
  vim.keymap.set("n", "<leader>qs", persistence.select, { desc = "Select" })
  vim.keymap.set("n", "<leader>qL", function()
    persistence.load({ last = true })
  end, { desc = "Load last(any directory)" })
  vim.keymap.set("n", "<leader>qd", persistence.stop, { desc = "Stop" })
end)
