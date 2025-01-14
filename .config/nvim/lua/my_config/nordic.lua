require('nordic').setup({
  transparent = { bg = true },
  -- Enable brighter float border.
  bright_border = true,
  on_highlight = function(highlights, palette)
    highlights.QuickScopePrimary = {
      fg = palette.yellow.bright,
      bg = palette.gray0,
      bold = true,
      underline = true,
    }
    highlights.QuickScopeSecondary = {
      fg = palette.orange.dim,
      bg = palette.gray0,
      bold = true,
      underline = true,
    }
    highlights['@tag.attribute'] = {
      fg = '#6cc3b7', -- #6dada4 |
    }
  end,
})
vim.cmd.colorscheme 'nordic'
