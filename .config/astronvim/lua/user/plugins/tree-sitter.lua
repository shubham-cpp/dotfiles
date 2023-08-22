return {
  'nvim-treesitter/nvim-treesitter',
  dependencies = {
    {
      'andymass/vim-matchup',
      init = function()
        vim.g.matchip_matchparen_deferred = 1
      end,
    },
    'nvim-treesitter/nvim-treesitter-textobjects',
  },
  opts = {
    highlight = {
      disable = function(lang, buf)
        local max_filesize = 100 * 1024 -- 100 KB
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if lang ~= 'help' and ok and stats and stats.size > max_filesize then
          return true
        end
      end,
      additional_vim_regex_highlighting = { 'markdown' },
    },
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
}
