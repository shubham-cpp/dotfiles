---@type LazySpec
return {
  'rebelot/heirline.nvim',
  enabled = false,
  event = 'VeryLazy',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require 'plugins.config.heirline'
  end,
}
