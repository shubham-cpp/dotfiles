---@type LazySpec
return {
  'rebelot/heirline.nvim',
  enabled = true,
  event = 'VeryLazy',
  dependencies = { 'nvim-tree/nvim-web-devicons', 'Zeioth/heirline-components.nvim' },
  config = function()
    require 'plugins.config.heirline'
  end,
}
