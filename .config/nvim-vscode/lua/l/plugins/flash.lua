---@type LazySpec
return {
  "folke/flash.nvim",
  opts = { modes = { char = { enabled = false, autohide = false } } },
  keys = function()
    local flash = require "flash"
    return {
      { "s", flash.jump, mode = { "n", "x" }, desc = "Flash" },
      { "S", flash.treesitter, mode = { "n", "o" }, desc = "Treesitter" },
      { "r", flash.remote, mode = "o", desc = "Remote" },
      { "<c-s>", flash.toggle, mode = "c", desc = "Toggle flash search" },
    }
  end,
}
