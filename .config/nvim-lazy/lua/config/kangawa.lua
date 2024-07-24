require("kanagawa").setup({
  compile = true,
  -- transparent = true,
  -- dimInactive = false,
  ---@type "wave" | "dragon" | "lotus"
  theme = "dragon",
  overrides = function(colors) -- add/modify highlights
    local theme = colors.theme
    return {
      Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 }, -- add `blend = vim.o.pumblend` to enable transparency
      PmenuSel = { fg = "NONE", bg = theme.ui.bg_p2 },
      PmenuSbar = { bg = theme.ui.bg_m1 },
      PmenuThumb = { bg = theme.ui.bg_p2 },
      QuickScopePrimary = { bold = true, underline = true, fg = colors.palette.carpYellow },
      QuickScopeSecondary = { bold = true, underline = true, fg = theme.syn.type },
    }
  end,
})
vim.cmd.colorscheme("kanagawa-dragon")
