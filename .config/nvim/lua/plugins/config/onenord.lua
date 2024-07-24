require('onenord').setup({
  fade_nc = true,
  disable = {
    background = true,
  },
  styles = {
    comments = 'italic',
    strings = 'NONE',
    keywords = 'NONE',
    functions = 'bold',
    variables = 'NONE',
    diagnostics = 'underline',
  },
  custom_highlights = {
    QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = 'underline,bold' },
    QuickScopeSecondary = {
      fg = '#61afef',
      bg = '#505050',
      style = 'underline,bold',
    },
    PmenuSel = { bg = '#61afef', fg = '#24253b' },
  },
})
vim.cmd 'colorscheme onenord'
