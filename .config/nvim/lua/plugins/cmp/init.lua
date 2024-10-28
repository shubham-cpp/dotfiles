---@type LazySpec
return {
  'iguanacucumber/magazine.nvim',
  name = 'nvim-cmp', -- Otherwise highlighting gets messed up
  enabled = true,
  event = 'BufReadPre',
  dependencies = {
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    -- 'hrsh7th/cmp-path',
    'https://codeberg.org/FelipeLema/cmp-async-path',
    'hrsh7th/cmp-cmdline',
    'lukas-reineke/cmp-rg',
    -- 'lukas-reineke/cmp-under-comparator',
    'octaltree/cmp-look',
    'saadparwaiz1/cmp_luasnip',
    {
      'L3MON4D3/LuaSnip',
      build = 'make install_jsregexp',
      dependencies = { 'rafamadriz/friendly-snippets' },
      event = 'User BaseFile',
      opts = { history = true, delete_check_events = 'TextChanged', region_check_events = 'CursorMoved' },
      config = function(_, opts)
        if opts then
          require('luasnip').config.setup(opts)
        end

        vim.tbl_map(function(type)
          require('luasnip.loaders.from_' .. type).lazy_load()
        end, { 'vscode', 'snipmate', 'lua' })

        local extends = {
          typescript = { 'tsdoc' },
          javascript = { 'jsdoc' },
          lua = { 'luadoc' },
          python = { 'pydoc' },
          rust = { 'rustdoc' },
          cs = { 'csharpdoc' },
          java = { 'javadoc' },
          c = { 'cdoc' },
          cpp = { 'cppdoc' },
          php = { 'phpdoc' },
          kotlin = { 'kdoc' },
          ruby = { 'rdoc' },
          sh = { 'shelldoc' },
        }
        -- friendly-snippets - enable standardized comments snippets
        for ft, snips in pairs(extends) do
          require('luasnip').filetype_extend(ft, snips)
        end
      end,
    },
    'onsails/lspkind.nvim',
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      opts = {
        library = { 'lazy.nvim', { path = 'luvit-meta/library', words = { 'vim%.uv' } } },
      },
    },
    { 'Bilal2453/luvit-meta', lazy = true },
  },
  config = function()
    require 'plugins.config.cmp'
  end,
}
