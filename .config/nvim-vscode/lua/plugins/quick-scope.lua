local config = {
  'unblevable/quick-scope',
  keys = { 'f', 'F', 't', 'T' },
  init = function()
    vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
    vim.g.qs_buftype_blacklist = { 'terminal', 'nofile', 'dashboard', 'startify', 'floatterm' }
    vim.g.qs_lazy_highlight = 1

    vim.api.nvim_set_hl(
      0,
      'QuickScopePrimary',
      { fg = '#afff5f', bold = true, underline = true, ctermfg = 'lightgreen' }
    )
    vim.api.nvim_set_hl(
      0,
      'QuickScopeSecondary',
      { fg = '#5fffff', bold = true, underline = true, ctermfg = 'lightblue' }
    )
  end,
}

return config
