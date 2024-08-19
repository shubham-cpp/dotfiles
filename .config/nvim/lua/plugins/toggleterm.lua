return {
  'akinsho/nvim-toggleterm.lua',
  version = '*',
  keys = { '<leader>tl', '<C-\\>' },
  config = function()
    require 'plugins.config.toggleterm'
  end,
}
