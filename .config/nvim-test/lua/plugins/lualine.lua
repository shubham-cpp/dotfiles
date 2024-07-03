---@type LazySpec
return {
  'nvim-lualine/lualine.nvim',
  enabled = true,
  event = 'VeryLazy',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require 'plugins.config.lualine'
  end,
}
