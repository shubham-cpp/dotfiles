---@type LazySpec
return {
  'nvim-lualine/lualine.nvim',
  enabled = false,
  event = 'VeryLazy',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require 'plugins.config.lualine'
  end,
}
