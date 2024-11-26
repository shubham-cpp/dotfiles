---@type LazySpec
return {
  'echasnovski/mini.ai',
  version = '*',
  event = { 'BufReadPost', 'BufNewFile' },
  enabled = false,
  dependencies = {
    {
      'echasnovski/mini.extra',
      version = '*',
      opts = {},
      config = function()
        require('mini.extra').setup()
      end,
    },
  },
  opts = function()
    local ai = require 'mini.ai'
    local gen_ai_spec = require('mini.extra').gen_ai_spec
    return {
      search_method = 'cover_or_prev',
      n_lines = 100,
      o = ai.gen_spec.treesitter({ -- code block
        a = { '@block.outer', '@conditional.outer', '@loop.outer' },
        i = { '@block.inner', '@conditional.inner', '@loop.inner' },
      }),
      f = ai.gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }), -- function
      c = ai.gen_spec.treesitter({ a = '@class.outer', i = '@class.inner' }), -- class
      t = { '<([%p%w]-)%f[^<%w][^<>]->.-</%1>', '^<.->().*()</[^/]->$' }, -- tags
      B = gen_ai_spec.buffer(),
      D = gen_ai_spec.diagnostic(),
      L = gen_ai_spec.line(),
    }
  end,
  config = function(_, opts)
    vim.print(opts)
    require('mini.ai').setup(opts)
  end,
}
