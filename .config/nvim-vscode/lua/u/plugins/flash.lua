local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "folke/flash.nvim" })
  local flash = require "flash"
  flash.setup({ modes = { char = { enabled = false, autohide = false } } })

  vim.keymap.set({ "n", "x" }, "s", flash.jump, { desc = "Flash" })
  vim.keymap.set({ "n", "o" }, "S", flash.treesitter, { desc = "Treesitter" })
  vim.keymap.set("o", "r", flash.remote, { desc = "Remote" })
  vim.keymap.set("c", "<c-s>", flash.toggle, { desc = "Toggle flash search" })
end)
