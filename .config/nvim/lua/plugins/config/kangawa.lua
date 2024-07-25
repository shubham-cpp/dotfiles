---@type "wave" | "dragon" | "lotus"
local themeName = 'dragon'
require('kanagawa').setup({
  compile = true,
  -- transparent = true, -- do not set background color
  -- dimInactive = true,
  ---@type "wave" | "dragon" | "lotus"
  theme = themeName,
  ---@type table<string, "wave" | "dragon" | "lotus">
  background = { -- map the value of 'background' option to a theme
    dark = themeName,
    light = 'lotus',
  },
  overrides = function(colors) -- add/modify highlights
    local theme = colors.theme
    return {
      Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 }, -- add `blend = vim.o.pumblend` to enable transparency
      PmenuSel = { fg = 'NONE', bg = theme.ui.bg_p2 },
      PmenuSbar = { bg = theme.ui.bg_m1 },
      PmenuThumb = { bg = theme.ui.bg_p2 },
    }
  end,
})
vim.cmd 'colorscheme kanagawa-wave'
local colors = require('kanagawa.colors').setup({ theme = themeName })
-- local palette_colors = colors.palette
vim.api.nvim_set_hl(0, 'QuickScopePrimary', {
  fg = colors.palette.autumnGreen,
  bg = colors.palette.sumiInk2,
  underline = true,
  bold = true,
})

vim.api.nvim_set_hl(0, 'QuickScopeSecondary', {
  fg = colors.palette.autumnYellow,
  bg = colors.palette.sumiInk2,
  underline = true,
  bold = true,
})
