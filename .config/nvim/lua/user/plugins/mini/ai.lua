---@type LazySpec
return {
  'echasnovski/mini.ai',
  event = 'VeryLazy',
  dependencies = {
    {
      'echasnovski/mini.extra',
      config = function()
        require('mini.extra').setup()
      end,
    },
    'nvim-treesitter',
  },
  opts = function()
    local ai = require 'mini.ai'
    local gen_ai_spec = require('mini.extra').gen_ai_spec
    return {
      n_lines = 500,
      custom_textobjects = {
        o = ai.gen_spec.treesitter({ -- code block
          a = { '@block.outer', '@conditional.outer', '@loop.outer' },
          i = { '@block.inner', '@conditional.inner', '@loop.inner' },
        }),
        f = ai.gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }), -- function
        c = ai.gen_spec.treesitter({ a = '@class.outer', i = '@class.inner' }), -- class
        t = { '<([%p%w]-)%f[^<%w][^<>]->.-</%1>', '^<.->().*()</[^/]->$' }, -- tags
        e = gen_ai_spec.buffer(),
        D = gen_ai_spec.diagnostic(),
        L = gen_ai_spec.line(),
      },
    }
  end,
  config = function(_, opts)
    require('mini.ai').setup(opts)
  end,
}
