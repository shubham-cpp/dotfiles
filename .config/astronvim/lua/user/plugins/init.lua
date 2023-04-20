local init = {
  { 'goolord/alpha-nvim',           enabled = false },
  { 'tweekmonster/startuptime.vim', cmd = 'StartupTime' },
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-cmdline',
      'hrsh7th/cmp-nvim-lua',
    },
    opts = function(_, opts)
      local cmp = require 'cmp'
      local sources = require 'cmp.config.sources'
      opts.mapping['<C-x><C-s>'] = cmp.mapping.complete({
        config = { sources = { { name = 'luasnip' } } },
      })
      opts.mapping['<C-x><C-f>'] = cmp.mapping.complete({
        config = { sources = { { name = 'path' } } },
      })
      opts.mapping['<CR>'] = cmp.mapping({
        i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
        c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
      })

      opts.sources = cmp.config.sources({
        { name = 'nvim_lsp',                priority = 1000 },
        { name = 'nvim_lua',                priority = 1000 },
        { name = 'nvim_lsp_signature_help', priority = 900 },
        { name = 'luasnip',                 priority = 750 },
        {
          name = 'buffer',
          priority = 300,
          option = {
            get_bufnrs = function()
              return vim.api.nvim_list_bufs()
            end,
          },
        },
        { name = 'path', priority = 250 },
      })
      -- opts.sorting = {
      --   priority_weight = 2,
      --   comparators = {
      --     cmp.config.compare.recently_used,
      --     cmp.config.compare.score,
      --     cmp.config.compare.locality,
      --     cmp.config.compare.offset,
      --     cmp.config.compare.order,
      --     cmp.config.compare.exact,
      --     cmp.config.compare.kind,
      --   },
      -- }

      cmp.setup.cmdline(':', {
        sources = sources({
          {
            name = 'cmdline',
            option = {
              ignore_cmds = { 'Man', '!' },
            },
          },
          { name = 'path' },
        }),
        mapping = cmp.mapping.preset.cmdline({}),
      })

      cmp.setup.cmdline('/', {
        sources = sources({
          { name = 'nvim_lsp_signature_help' },
          { name = 'buffer',                 keyword_pattern = [=[[^[:blank:]].*]=] },
        }),
        mapping = cmp.mapping.preset.cmdline({}),
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      {
        'andymass/vim-matchup',
        init = function()
          vim.g.matchup_matchparen_deferred = 1
        end,
      },
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    opts = {
      auto_install = vim.fn.executable 'tree-sitter' == 1,
      highlight = { disable = { 'help' } },
      matchup = { enable = true },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            aA = '@attribute.outer',
            iA = '@attribute.inner',
            aB = '@block.outer',
            iB = '@block.inner',
            aD = '@conditional.outer',
            iD = '@conditional.inner',
            aF = '@function.outer',
            iF = '@function.inner',
            aL = '@loop.outer',
            iL = '@loop.inner',
            aP = '@parameter.outer',
            iP = '@parameter.inner',
            aR = '@regex.outer',
            iR = '@regex.inner',
            aX = '@class.outer',
            iX = '@class.inner',
            aS = '@statement.outer',
            iS = '@statement.outer',
            aN = '@number.inner',
            iN = '@number.inner',
            aC = '@comment.outer',
            iC = '@comment.outer',
          },
        },
        move = {
          enable = true,
          set_jumps = true,
          goto_next_start = {
            [']b'] = { query = '@block.outer', desc = 'Next block start' },
            [']f'] = { query = '@function.outer', desc = 'Next function start' },
            [']p'] = { query = '@parameter.outer', desc = 'Next parameter start' },
            [']x'] = { query = '@class.outer', desc = 'Next class start' },
            [']c'] = { query = '@comment.outer', desc = 'Next comment start' },
          },
          goto_next_end = {
            [']B'] = { query = '@block.outer', desc = 'Next block end' },
            [']F'] = { query = '@function.outer', desc = 'Next function end' },
            [']P'] = { query = '@parameter.outer', desc = 'Next parameter end' },
            [']X'] = { query = '@class.outer', desc = 'Next class end' },
            [']C'] = { query = '@comment.outer', desc = 'Next comment end' },
          },
          goto_previous_start = {
            ['[b'] = { query = '@block.outer', desc = 'Previous block start' },
            ['[f'] = { query = '@function.outer', desc = 'Previous function start' },
            ['[p'] = { query = '@parameter.outer', desc = 'Previous parameter start' },
            ['[x'] = { query = '@class.outer', desc = 'Previous class start' },
            ['[c'] = { query = '@comment.outer', desc = 'Previous comment start' },
          },
          goto_previous_end = {
            ['[B'] = { query = '@block.outer', desc = 'Previous block end' },
            ['[F'] = { query = '@function.outer', desc = 'Previous function end' },
            ['[P'] = { query = '@parameter.outer', desc = 'Previous parameter end' },
            ['[X'] = { query = '@class.outer', desc = 'Previous class end' },
            ['[C'] = { query = '@comment.outer', desc = 'Previous comment end' },
          },
        },
        swap = {
          enable = true,
          swap_next = {
            ['>B'] = { query = '@block.outer', desc = 'Swap next block' },
            ['>F'] = { query = '@function.outer', desc = 'Swap next function' },
            ['>P'] = { query = '@parameter.inner', desc = 'Swap next parameter' },
          },
          swap_previous = {
            ['<B'] = { query = '@block.outer', desc = 'Swap previous block' },
            ['<F'] = { query = '@function.outer', desc = 'Swap previous function' },
            ['<P'] = { query = '@parameter.inner', desc = 'Swap previous parameter' },
          },
        },
      },
    },
  },
}
return init
