---@type LazySpec
return {
  {
    "vague2k/vague.nvim",
    enabled = true,
    priority = 1000,
    opts = { transparent = false },
    config = function(_, opts)
      local vague = require "vague"
      vague.setup(opts)
      local palette = vague.get_palette()

      vim.cmd.colorscheme "vague"
      -- vim.api.nvim_set_hl(0, 'StatusLine', { bg = '#181818' })

      vim.api.nvim_set_hl(0, "@tag.attribute", { fg = palette.property })
      vim.api.nvim_set_hl(0, "WinBar", { bg = palette.bg })
      vim.api.nvim_set_hl(0, "SnacksPickerMatch", { fg = palette.delta, bold = true })
      vim.api.nvim_set_hl(
        0,
        "QuickScopePrimary",
        { fg = palette.delta, bg = palette.visual, bold = true, undercurl = true }
      )
      vim.api.nvim_set_hl(
        0,
        "QuickScopeSecondary",
        { fg = palette.hint, bg = palette.visual, bold = true, undercurl = true }
      )
    end,
  },
  {
    "EdenEast/nightfox.nvim",
    enabled = false,
    priority = 1000,
    opts = {
      options = {
        transparent = true,
        styles = { comments = "italic" },
      },
    },
    config = function(_, opts)
      require("nightfox").setup(opts)
      vim.cmd.colorscheme "duskfox"
    end,
  },
}
