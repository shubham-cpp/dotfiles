local config = {
  'rmehri01/onenord.nvim',
  enabled = not vim.g.vscode,
  opts = {
    fade_nc = true,
    styles = {
      comments = 'italic',
      strings = 'NONE',
      keywords = 'NONE',
      functions = 'bold',
      variables = 'NONE',
      diagnostics = 'underline',
    },
    custom_highlights = {
      Normal = { bg = '#1e222a' },
      NormalNC = { bg = '#1d1915' },
      -- QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = 'underline,bold' },
      -- QuickScopeSecondary = {
      --   fg = '#61afef',
      --   bg = '#505050',
      --   style = 'underline,bold',
      -- },
    },
  },
}
return config
