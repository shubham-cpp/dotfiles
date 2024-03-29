local config = {
  {
    'nvim-treesitter/nvim-treesitter',
    version = false, -- last release is way too old and doesn't work on Windows
    build = ':TSUpdate',
    -- event = 'VeryLazy',
    event = { 'BufReadPost', 'BufNewFile' },
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
      { 'nvim-treesitter/playground', cmd = 'TSPlaygroundToggle' },
      'andymass/vim-matchup',
      'windwp/nvim-ts-autotag',
    },
    init = function()
      vim.g.matchup_matchparen_offscreen = { method = 'popup' }
    end,
    config = function()
      require('nvim-treesitter.configs').setup({
        highlight = { enable = true, additional_vim_regex_highlighting = { 'markdown', 'xml' } },
        matchup = { enable = true },
        autotag = { enable = true, filetypes = { 'html', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'svelte'} },
        ensure_installed = {
          'bash',
          'c',
          'comment',
          'cpp',
          'css',
          'dockerfile',
          'fish',
          'go',
          'gomod',
          'gosum',
          'gowork',
          'html',
          'javascript',
          'jsdoc',
          'json',
          'json5',
          'jsonc',
          'lua',
          'luadoc',
          'markdown',
          'markdown_inline',
          'python',
          'query',
          'regex',
          'rasi',
          'rust',
          'scss',
          'sxhkdrc',
          'svelte',
          'tsx',
          'todotxt',
          'typescript',
          'vim',
          'vimdoc',
          'vue',
          'yaml',
        },
        query_linter = {
          enable = true,
          use_virtual_text = true,
          lint_events = { 'BufWrite', 'CursorHold' },
        },
        playground = {
          enable = true,
          disable = {},
          updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
          persist_queries = false, -- Whether the query persists across vim sessions
          keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ['af'] = '@function.outer',
              ['if'] = '@function.inner',
              ['ac'] = '@class.outer',
              ['ic'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
              ['al'] = { query = '@scope', query_group = 'locals', desc = 'Select language scope' },
            },
            selection_modes = {
              ['@parameter.outer'] = 'v', -- charwise
              ['@function.outer'] = 'V', -- linewise
              ['@class.outer'] = '<c-v>', -- blockwise
            },
            include_surrounding_whitespace = true,
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              [']m'] = '@function.outer',
              [']]'] = { query = '@class.outer', desc = 'Next class start' },
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
              ['[['] = '@class.outer',
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
              ['>K'] = { query = '@block.outer', desc = 'Swap next block' },
              ['>F'] = { query = '@function.outer', desc = 'Swap next function' },
              ['>A'] = { query = '@parameter.inner', desc = 'Swap next argument' },
            },
            swap_previous = {
              ['<K'] = { query = '@block.outer', desc = 'Swap previous block' },
              ['<F'] = { query = '@function.outer', desc = 'Swap previous function' },
              ['<A'] = { query = '@parameter.inner', desc = 'Swap previous argument' },
            },
          },
        },
      })
    end,
  },
  -- {
  --   'windwp/nvim-ts-autotag',
  --   version = false,
  --   event = { 'BufReadPost', 'BufNewFile' },
  --   dependencies = 'nvim-treesitter/nvim-treesitter',
  --   config = function()
  --     require('nvim-ts-autotag').setup({ filetypes = { 'html', 'javascript', 'javascriptreact', 'typescriptreact' } })
  --   end,
  -- },
}
return config
