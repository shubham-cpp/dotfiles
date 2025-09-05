---@type LazySpec
return {
  {
    "NAlexPear/Spacegray.nvim",
    enabled = false,
    lazy = true,
    init = function()
      vim.cmd.colorscheme "spacegray"
    end,
  },
  {
    "AlexvZyl/nordic.nvim",
    lazy = true,
    enabled = vim.g.vscode == nil,
    init = function()
      vim.cmd.colorscheme "nordic"
    end,
    opts = { transparent = { bg = true } },
  },
  {
    "blazkowolf/gruber-darker.nvim",
    enabled = false,
    lazy = true,
    config = function()
      require("gruber-darker").setup()
      vim.cmd.colorscheme "gruber-darker"

      local c = require "gruber-darker.palette"

      vim.api.nvim_set_hl(0, "MatchParenCur", { bold = true })
      vim.api.nvim_set_hl(0, "MiniPickMatchCurrent", { link = "TelescopeMatching" })
      vim.api.nvim_set_hl(0, "@lsp.type.keyword.lua", { fg = c["brown"]:to_string(), bold = false })
      vim.api.nvim_set_hl(0, "@lsp.type.event.lua", { link = "GruberDarkerYellowBold" })
      vim.api.nvim_set_hl(
        0,
        "QuickScopePrimary",
        { fg = c["yellow"]:to_string(), bg = c["bg+1"]:to_string(), underline = true, bold = true }
      )
      vim.api.nvim_set_hl(
        0,
        "QuickScopeSecondary",
        { fg = c["brown"]:to_string(), bg = c["bg+1"]:to_string(), underline = true, bold = true }
      )
    end,
  },
}
