require('gruvbox').setup({
  undercurl = true,
  underline = true,
  bold = true,
  italic = true,
  strikethrough = true,
  invert_selection = false,
  invert_signs = false,
  invert_tabline = false,
  invert_intend_guides = true,
  inverse = true, -- invert background for search, diffs, statuslines and errors
  contrast = 'hard', -- can be "hard", "soft" or empty string
  overrides = {
    QuickScopePrimary = { fg = '#282829', bg = '#fabd2e', bold = true, underline = true, undercurl = true },
    QuickScopeSecondary = {
      fg = '#282829',
      bg = '#ff9900',
      bold = true,
      underline = true,
      undercurl = true,
    },
  },
})
vim.cmd('colorscheme gruvbox')
