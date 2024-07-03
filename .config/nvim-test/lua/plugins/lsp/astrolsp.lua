---@type LazySpec
return {
  'AstroNvim/astrolsp',
  lazy = true,
  dependencies = {
    {
      'SmiteshP/nvim-navic',
      init = function()
        vim.g.navic_silence = true
      end,
      opts = {
        highlight = true,
        depth_limit = 5,
        lazy_update_context = true,
      },
    },
    'williamboman/mason-lspconfig.nvim',
    'hrsh7th/nvim-cmp',
  },
  opts = require('plugins.config.astrolsp').opts,
  -- config = require('plugins.config.astrolsp').config,
}
