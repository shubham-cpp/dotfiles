---@type LazySpec
return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  opts_extend = { "spec", "disable.ft", "disable.bt" },
  opts = {
    ---@type false | "classic" | "modern" | "helix"
    preset = "modern",
    -- icons = { rules = false, separator = "-" },
  },
  config = function(_, opts)
    local wk = require "which-key"
    wk.setup(opts)
    wk.add {
      { "<leader>f", group = "file" }, -- group
      { "<leader>l", group = "lsp" }, -- group
      { "<leader>m", group = "multi-cursors" }, -- group
      { "<leader>n", group = "neogen" }, -- group
      { "<leader>u", group = "toggles" }, -- group
      { "<leader>b", group = "buffers" }, -- group
      { "<leader>g", group = "git" }, -- group
      { "<leader>q", group = "session" }, -- group
    }
  end,
}
