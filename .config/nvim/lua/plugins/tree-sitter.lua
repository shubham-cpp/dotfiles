---@type LazySpec
return {
  {
    'nvim-treesitter/nvim-treesitter',
    event = { 'BufReadPre', 'BufNewFile' },
    lazy = vim.fn.argc(-1) == 0,
    version = false, -- last release is way too old and doesn't work on Windows
    build = ':TSUpdate',
    init = function()
      require 'nvim-treesitter.query_predicates'
    end,
    cmd = { 'TSUpdateSync', 'TSUpdate', 'TSInstall' },
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
      'andymass/vim-matchup',
      'windwp/nvim-ts-autotag',
      {
        'nvim-treesitter/nvim-treesitter-context',
        opts = {
          mode = 'cursor',
          max_lines = 3,
        },
      },
    },
    config = function()
      require 'plugins.config.tree-sitter'
    end,
  },
  {
    'Wansmer/treesj',
    keys = {
      { '<localleader>m', '<cmd>TSJToggle<cr>' },
      {
        '<localleader>M',
        function()
          require('treesj').toggle({ split = { recursive = true } })
        end,
        desc = 'Toggle Treesitter Join(recursive)',
      },
      { '<localleader>j', '<cmd>TSJJoin<cr>' },
      { '<localleader>s', '<cmd>TSJSplit<cr>' },
    },
    dependencies = { 'nvim-treesitter/nvim-treesitter' }, -- if you install parsers with `nvim-treesitter`
    cmd = { 'TSJToggle', 'TSJSplit', 'TSJJoin' },
    opts = {
      use_default_keymaps = false,
    },
    config = function(_, opts)
      require('treesj').setup(opts)
    end,
  },
}
