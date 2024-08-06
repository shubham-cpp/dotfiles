---@type LazySpec
return {
  'hrsh7th/nvim-cmp',
  event = 'BufReadPre',
  version = false,
  commit = 'b356f2c',
  pin = true,
  -- commit = 'b356f2c',
  dependencies = {
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    {
      'hrsh7th/vim-vsnip',
      version = false,
      dependencies = {
        { 'hrsh7th/cmp-vsnip', version = false },
        { 'rafamadriz/friendly-snippets', version = false },
      },
    },
    -- {
    --   'L3MON4D3/LuaSnip',
    --   version = 'v2.*',
    --   run = 'make install_jsregexp',
    --   dependencies = {
    --     'saadparwaiz1/cmp_luasnip',
    --     'rafamadriz/friendly-snippets',
    --   },
    --   config = function()
    --     -- require'luasnip'.filetype_extend("ruby", {"rails"})
    --     require('luasnip.loaders.from_vscode').lazy_load()
    --     -- require('luasnip.loaders.from_snipmate').lazy_load()
    --   end,
    -- },
    'onsails/lspkind.nvim',
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      opts = {
        library = {
          'lazy.nvim',
          { path = 'luvit-meta/library', words = { 'vim%.uv' } },
        },
      },
    },
    { 'Bilal2453/luvit-meta', lazy = true },
  },
  config = function()
    require 'plugins.config.cmp'
  end,
}
