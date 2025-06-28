---@type LazySpec
return {
  {
    "echasnovski/mini.operators",
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gS", mode = { "n", "x" }, desc = "Sort" },
      "X",
    },
    opts = {
      -- Exchange text regions
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
      sort = { prefix = "gS" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
  {
    "echasnovski/mini.move",
    keys = {
      { "<", mode = { "n", "v" } },
      { ">", mode = { "n", "v" } },
      { "J", mode = "v" },
      { "K", mode = "v" },
    },
    opts = {
      mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = "<",
        right = ">",
        down = "J",
        up = "K",

        -- Move current line in Normal mode
        line_left = "<",
        line_right = ">",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
    -- config = function(_, opts) require("mini.move").setup(opts) end,
  },
  {
    "echasnovski/mini.align",
    keys = { { "ga", mode = { "n", "x" } }, { "gA", mode = { "n", "x" } } },
    opts = {},
  },
  { "echasnovski/mini.extra", opts = {}, lazy = true },
  {
    "echasnovski/mini.ai",
    event = "User AstroFile",
    opts = function()
      local ai = require "mini.ai"
      local gen_ai_spec = require("mini.extra").gen_ai_spec
      return {
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter { -- code block
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          },
          f = ai.gen_spec.treesitter { a = "@function.outer", i = "@function.inner" }, -- function
          c = ai.gen_spec.treesitter { a = "@class.outer", i = "@class.inner" }, -- class
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" }, -- tags
          d = gen_ai_spec.number(), -- digits
          e = { -- Word with case
            { "%u[%l%d]+%f[^%l%d]", "%f[%S][%l%d]+%f[^%l%d]", "%f[%P][%l%d]+%f[^%l%d]", "^[%l%d]+%f[^%l%d]" },
            "^().*()$",
          },
          g = gen_ai_spec.buffer(), -- buffer
          D = gen_ai_spec.diagnostic(),
          u = ai.gen_spec.function_call(), -- u for "Usage"
          U = ai.gen_spec.function_call { name_pattern = "[%w_]" }, -- without dot in function name
        },
      }
    end,
    specs = {
      {
        "catppuccin",
        optional = true,
        ---@type CatppuccinOptions
        opts = { integrations = { mini = true } },
      },
    },
  },
}
