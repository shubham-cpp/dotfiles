return {
  'nvim-telescope/telescope.nvim',
  cmd = 'Telescope',
  enabled = false,
  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make', enabled = false },
    'natecraddock/telescope-zf-native.nvim',
    'nvim-telescope/telescope-ui-select.nvim',
    { 'fdschmidt93/telescope-egrepify.nvim', dependencies = 'nvim-lua/plenary.nvim' },
  },
  keys = require('plugins.config.telescope').keys,
  config = require('plugins.config.telescope').config,
}
