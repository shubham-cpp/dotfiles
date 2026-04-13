-- mini.nvim modules
return {
  {
    url = "nvim-mini/mini.align",
    config = function()
      require("mini.align").setup()
    end,
  },
  {
    url = "nvim-mini/mini.move",
    config = function()
      require("mini.move").setup({
        mappings = {
          left = "<",
          right = ">",
          down = "J",
          up = "K",
          line_left = "<M-h>",
          line_right = "<M-l>",
          line_down = "<M-j>",
          line_up = "<M-k>",
        },
      })
    end,
  },
  {
    url = "nvim-mini/mini.indentscope",
    config = function()
      require("mini.indentscope").setup({
        draw = {
          animation = function()
            return 0
          end,
        },
      })
    end,
  },
  {
    url = "nvim-mini/mini.icons",
    config = function()
      require("mini.icons").setup()
      require("mini.icons").mock_nvim_web_devicons()
    end,
  },
  {
    url = "nvim-mini/mini.notify",
    config = function()
      require("mini.notify").setup()
      vim.notify = require("mini.notify").make_notify()
    end,
    keys = {
      {
        "<Leader>on",
        function()
          require("mini.notify").show_history()
        end,
        desc = "Notification History",
      },
    },
  },
  {
    url = "nvim-mini/mini.comment",
    config = function()
      require("mini.comment").setup({
        options = {
          custom_commentstring = function()
            return require("ts_context_commentstring").calculate_commentstring() or vim.bo.commentstring
          end,
        },
      })
    end,
  },
  {
    url = "nvim-mini/mini.operators",
    config = function()
      require("mini.operators").setup({
        evaluate = { prefix = "g=" },
        exchange = { prefix = "ge" },
        multiply = { prefix = "gm" },
        replace = { prefix = "x" },
        sort = { prefix = "gs" },
      })
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
  {
    url = "nvim-mini/mini.extra",
  },
  {
    url = "nvim-mini/mini.ai",
    config = function()
      local ai = require("mini.ai")
      local gen_ai_spec = require("mini.extra").gen_ai_spec
      ai.setup({
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter({
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
          c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }),
          ["/"] = ai.gen_spec.treesitter({ a = "@comment.outer", i = "@comment.inner" }),
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" },
          d = { "%f[%d]%d+" },
          C = {
            { "%u[%l%d]+%f[^%l%d]", "%f[%S][%l%d]+%f[^%l%d]", "%f[%P][%l%d]+%f[^%l%d]", "^[%l%d]+%f[^%l%d]" },
            "^().*()$",
          },
          e = function(ai_type)
            local from = { line = 1, col = 1 }
            local to = {
              line = vim.fn.line("$"),
              col = math.max(vim.fn.getline("$"):len(), 1),
            }
            if ai_type == "i" then
              local first = vim.fn.nextnonblank(1)
              local last = vim.fn.prevnonblank(vim.fn.line("$"))
              from = { line = first, col = 1 }
              to = { line = last, col = math.max(vim.fn.getline(last):len(), 1) }
            end
            return { from = from, to = to }
          end,
          u = ai.gen_spec.function_call(),
          U = ai.gen_spec.function_call({ name_pattern = "[%w_]" }),
          B = gen_ai_spec.buffer(),
          D = gen_ai_spec.diagnostic(),
          I = gen_ai_spec.indent(),
          L = gen_ai_spec.line(),
          N = gen_ai_spec.number(),
        },
      })
    end,
  },
}
