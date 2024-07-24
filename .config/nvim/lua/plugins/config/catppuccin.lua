require('catppuccin').setup({
  ---@type "latte"| "frappe"| "macchiato"| "mocha"|"auto"
  flavour = 'mocha',
  transparent_background = true,
  dim_inactive = {
    enabled = false, -- dims the background color of inactive window
    shade = 'dark',
    percentage = 0.15, -- percentage of the shade to apply to the inactive window
  },
  custom_highlights = function(colors)
    return {
      QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = { 'underline', 'bold' } },
      QuickScopeSecondary = { fg = '#61afef', bg = '#505050', style = { 'underline', 'bold' } },
    }
  end,
})
vim.cmd 'colorscheme catppuccin'
