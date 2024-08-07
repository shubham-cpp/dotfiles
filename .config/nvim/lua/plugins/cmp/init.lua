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
    'saadparwaiz1/cmp_luasnip',
    {
      'L3MON4D3/LuaSnip',
      build = 'make install_jsregexp',
      dependencies = {
        'rafamadriz/friendly-snippets',
      },
      event = 'User BaseFile',
      opts = {
        history = true,
        delete_check_events = 'TextChanged',
        region_check_events = 'CursorMoved',
      },
      config = function(_, opts)
        if opts then
          require('luasnip').config.setup(opts)
        end
        vim.tbl_map(function(type)
          require('luasnip.loaders.from_' .. type).lazy_load()
        end, { 'vscode', 'snipmate', 'lua' })
        -- friendly-snippets - enable standardized comments snippets
        require('luasnip').filetype_extend('typescript', { 'tsdoc' })
        require('luasnip').filetype_extend('javascript', { 'jsdoc' })
        require('luasnip').filetype_extend('lua', { 'luadoc' })
        require('luasnip').filetype_extend('python', { 'pydoc' })
        require('luasnip').filetype_extend('rust', { 'rustdoc' })
        require('luasnip').filetype_extend('cs', { 'csharpdoc' })
        require('luasnip').filetype_extend('java', { 'javadoc' })
        require('luasnip').filetype_extend('c', { 'cdoc' })
        require('luasnip').filetype_extend('cpp', { 'cppdoc' })
        require('luasnip').filetype_extend('php', { 'phpdoc' })
        require('luasnip').filetype_extend('kotlin', { 'kdoc' })
        require('luasnip').filetype_extend('ruby', { 'rdoc' })
        require('luasnip').filetype_extend('sh', { 'shelldoc' })
      end,
    },
    -- {
    --   'hrsh7th/vim-vsnip',
    --   version = false,
    --   dependencies = {
    --     { 'hrsh7th/cmp-vsnip', version = false },
    --     { 'rafamadriz/friendly-snippets', version = false },
    --   },
    -- },
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
