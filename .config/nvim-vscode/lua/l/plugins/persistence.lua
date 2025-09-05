---@type LazySpec
return {
  "folke/persistence.nvim",
  lazy = false,
  enabled = vim.g.vscode == nil,
  opts = {},
  keys = function()
    local persistence = require "persistence"
    return {
      { "<leader>qd", persistence.stop, desc = "Stop" },
      { "<leader>qs", persistence.select, desc = "Select" },
      { "<leader>ql", persistence.load, desc = "Load current directory" },
      {
        "<leader>qL",
        function()
          persistence.load({ last = true })
        end,
        desc = "Load last(any directory)",
      },
    }
  end,
}
