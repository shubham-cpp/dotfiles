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
      local configs = require 'nvim-treesitter.configs'
      ---@diagnostic disable-next-line: missing-fields
      configs.setup({
        highlight = {
          enable = true,
          -- additional_vim_regex_highlighting = { 'markdown', 'xml' },
          disable = function(_, bufnr)
            local line_count = vim.api.nvim_buf_line_count(bufnr)
            if line_count > 2500 then
              return true
            end
          end,
        },
        indent = { enable = false },
        matchup = { enable = true },
        ensure_installed = {
          'bash',
          'dockerfile',
          'fish',
          'query',
          'regex',
          'rasi',
          'sxhkdrc',
          'vim',
          'vimdoc',
          'diff',
          'git_config',
          'git_rebase',
          'gitattributes',
          'gitcommit',
          'gitignore',
          'hyprlang',
          'ini',
          'sql',
          'ssh_config',
          'tmux',
          'toml',
          'xml',
          'zathurarc',
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = '<C-space>',
            node_incremental = '<C-space>',
            scope_incremental = false,
            node_decremental = '<bs>',
          },
        },
        textobjects = {
          -- select = {
          --   enable = true,
          --   lookahead = true,
          --   keymaps = {
          --     ['af'] = '@function.outer',
          --     ['if'] = '@function.inner',
          --     ['ac'] = '@class.outer',
          --     ['ic'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
          --     ['al'] = { query = '@scope', query_group = 'locals', desc = 'Select language scope' },
          --     ['a/'] = { query = '@comment.outer', desc = 'Select outer comment' },
          --     ['i/'] = { query = '@comment.inner', desc = 'Select inner comment' },
          --   },
          --   selection_modes = {
          --     ['@parameter.outer'] = 'v', -- charwise
          --     ['@function.outer'] = 'V', -- linewise
          --     ['@class.outer'] = '<c-v>', -- blockwise
          --   },
          --   include_surrounding_whitespace = true,
          -- },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              [']m'] = '@function.outer',
              [']C'] = { query = '@class.outer', desc = 'Next class start' },
              [']l'] = '@loop.*',
              -- ["]o"] = { query = { "@loop.inner", "@loop.outer" } }
              [']s'] = { query = '@scope', query_group = 'locals', desc = 'Next scope' },
              [']z'] = { query = '@fold', query_group = 'folds', desc = 'Next fold' },
            },
            goto_next_end = {
              [']M'] = '@function.outer',
              [']['] = '@class.outer',
            },
            goto_previous_start = {
              ['[m'] = '@function.outer',
              ['[C'] = '@class.outer',
            },
            goto_previous_end = {
              ['[M'] = '@function.outer',
              ['[]'] = '@class.outer',
            },
            goto_next = {
              [']i'] = '@conditional.outer',
            },
            goto_previous = {
              ['[i'] = '@conditional.outer',
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ['<localleader>k'] = { query = '@block.outer', desc = 'Swap next block' },
              ['<localleader>f'] = { query = '@function.outer', desc = 'Swap next function' },
              ['<localleader>a'] = { query = '@parameter.inner', desc = 'Swap next argument' },
            },
            swap_previous = {
              ['<localleader>K'] = { query = '@block.outer', desc = 'Swap previous block' },
              ['<localleader>F'] = { query = '@function.outer', desc = 'Swap previous function' },
              ['<localleader>A'] = { query = '@parameter.inner', desc = 'Swap previous argument' },
            },
          },
        },
      })

      require('nvim-ts-autotag').setup({
        -- Defaults
        opts = {
          enable_close = true, -- Auto close tags
          enable_rename = true, -- Auto rename pairs of tags
          enable_close_on_slash = false, -- Auto close on trailing </
        },
        aliases = {
          heex = 'html',
        },
      })

      vim.keymap.set('n', '<LocalLeader>c', function()
        require('treesitter-context').go_to_context(vim.v.count1)
      end, { silent = true, desc = 'Goto Context' })
      vim.keymap.set('n', '<leader><up>', function()
        require('treesitter-context').go_to_context(vim.v.count1)
      end, { silent = true, desc = 'Goto Context' })

      -- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
      --   dofile(vim.g.base46_cache .. v)
      -- end
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
