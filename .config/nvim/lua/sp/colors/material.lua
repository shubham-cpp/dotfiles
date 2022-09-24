-- Options: deep ocean | darker | palenight | oceanic
vim.g.material_style = 'deep ocean'
vim.g.material_disable_background = true
local colors = require('material.colors')

require('material').setup({
  custom_highlights = {
    QuickScopePrimary = { fg = colors.orange, style = 'underline,italic' },
    QuickScopeSecondary = { fg = colors.cyan, style = 'underline,italic' },
    TSConstructor = { fg = colors.darkpurple, style = 'bold' },
  },
  contrast = {
    floating_windows = true,
    popup_menu = true,
    sign_column = true,
  },
  italics = {
    comments = true,
    keywords = true,
    -- functions = "none",
    -- strings = "none",
    -- variables = "none",
  },
  disable = {
    background = true,
  },
  lualine_style = 'stealth',
})
vim.cmd('colorscheme material')
