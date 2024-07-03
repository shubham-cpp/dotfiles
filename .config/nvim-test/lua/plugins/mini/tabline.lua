---@type LazySpec
return {
  'echasnovski/mini.tabline',
  enabled = false,
  event = 'VeryLazy',
  dependencies = { 'nvim-tree/nvim-web-devicons', 'tiagovla/scope.nvim' },
  config = function()
    require('mini.tabline').setup({})
  end,
}
