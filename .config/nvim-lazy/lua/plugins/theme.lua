---@type LazySpec
return {
  {
    "LazyVim",
    opts = { colorscheme = "vague" },
  },
  {
    "vague2k/vague.nvim",
    enabled = true,
    -- branch = "issue-20",
    opts = { transparent = false },
    config = function(_, opts)
      require("vague").setup(opts)
      local c = require("vague").get_palette()

      vim.api.nvim_set_hl(0, "@tag.attribute", { fg = c.property })
      vim.api.nvim_set_hl(0, "WinBar", { bg = "#141415" })
      vim.api.nvim_set_hl(0, "SnacksPickerMatch", { fg = c.delta, bold = true })
      vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = c.delta, bg = c.visual, bold = true, undercurl = true })
      vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = c.hint, bg = c.visual, bold = true, undercurl = true })
    end,
  },
}
