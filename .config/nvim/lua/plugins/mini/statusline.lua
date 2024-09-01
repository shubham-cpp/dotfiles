---@type LazySpec
return {
  'echasnovski/mini.statusline',
  enabled = false,
  event = 'VeryLazy',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require('mini.statusline').setup({})
  end,
}
