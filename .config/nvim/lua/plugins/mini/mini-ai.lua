local ai = require "mini.ai"
local gen_ai_spec = require("mini.extra").gen_ai_spec
ai.setup({
  n_lines = 100,
  search_method = "cover_or_nearest",
  custom_textobjects = {
    B = gen_ai_spec.buffer(),
    D = gen_ai_spec.diagnostic(),
    I = gen_ai_spec.indent(),
    l = gen_ai_spec.line(),
    N = gen_ai_spec.number(),
    o = ai.gen_spec.treesitter({ -- code block
      a = { "@block.outer", "@conditional.outer", "@loop.outer" },
      i = { "@block.inner", "@conditional.inner", "@loop.inner" },
    }),
    f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }), -- function
    c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }), -- class
    u = ai.gen_spec.function_call(), -- u for "Usage"
    U = ai.gen_spec.function_call({ name_pattern = "[%w_]" }), -- without dot in function name
    -- Jumps
    k = ai.gen_spec.treesitter({
      i = { "@assignment.lhs", "@key.inner" },
      a = { "@assignment.outer", "@key.inner" },
    }),
    -- Scope
    s = ai.gen_spec.treesitter({
      a = { "@function.outer", "@class.outer", "@testitem.outer" },
      i = { "@function.inner", "@class.inner", "@testitem.inner" },
    }),
    -- Value
    v = ai.gen_spec.treesitter({
      i = { "@assignment.rhs", "@value.inner", "@return.inner" },
      a = { "@assignment.outer", "@value.inner", "@return.outer" },
    }),
  },
})
