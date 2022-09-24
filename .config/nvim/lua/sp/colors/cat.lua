require('catppuccin').setup({
  -- transparent_background = true,
  styles = {
    comments = 'italic',
    functions = 'italic',
    keywords = 'italic',
    strings = 'NONE',
    variables = 'NONE',
  },
  integrations = {
    nvimtree = {
      enabled = true,
      show_root = true,
      transparent_panel = true,
    },
    which_key = true,
    indent_blankline = {
      enabled = true,
      colored_indent_levels = true,
    },
    neogit = true,
    hop = true,
  },
})
-- local colors = require('catppuccin.api.colors').get_colors()
-- local cat = require('catppuccin')
-- cat.remap({ QuickScopePrimary = { fg = colors.red } })
-- cat.remap({ QuickScopeSecondary = { fg = colors.green } })
vim.g.catppuccin_flavour = 'mocha' -- latte, frappe, macchiato, mocha
vim.cmd('colorscheme catppuccin')
