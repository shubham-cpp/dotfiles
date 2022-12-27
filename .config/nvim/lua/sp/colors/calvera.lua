vim.g.calvera_italic_keywords = false
vim.g.calvera_borders = true
vim.g.calvera_contrast = true
vim.g.calvera_hide_eob = true
-- vim.g.calvera_custom_colors = {contrast = "#0f111a"}

vim.cmd.colorscheme 'calvera'
vim.api.nvim_set_hl(
  0,
  'QuickScopePrimary',
  { ctermfg = 'lightgreen', ctermbg = 'black', fg = '#c3e88d', bg = '#0c0c1f', bold = true, underline = true }
)
vim.api.nvim_set_hl(
  0,
  'QuickScopeSecondary',
  { ctermfg = 'lightblue', ctermbg = 'black', fg = '#82aaff', bg = '#0c0c1f', bold = true, underline = true }
)
