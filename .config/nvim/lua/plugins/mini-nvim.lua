---@type LazySpec
return {
  {
    "echasnovski/mini.icons",
    version = "*",
    opts = function(_, opts)
      opts = opts or {}
      opts.file = opts.file or {}
      --- Taken from astronvim
      opts.filetype = vim.tbl_extend("force", opts.filetype or {}, {
        postcss = { glyph = "󰌜", hl = "MiniIconsOrange" },
        gotmpl = { glyph = "󰟓", hl = "MiniIconsGrey" },
      })

      local eslint_files = {
        -- ESLint <=8 (Deprecated)
        ".eslintignore",
        ".eslintrc",
        ".eslintrc.cjs",
        ".eslintrc.js",
        ".eslintrc.json",
        ".eslintrc.yaml",
        ".eslintrc.yml",
        -- ESLint >=9
        "eslint.config.cjs",
        "eslint.config.cts",
        "eslint.config.js",
        "eslint.config.mjs",
        "eslint.config.mts",
        "eslint.config.ts",
      }
      for _, filename in ipairs(eslint_files) do
        opts.file[filename] = { glyph = "󰱺", hl = "MiniIconsYellow" }
      end

      opts.file[".go-version"] = { glyph = "", hl = "MiniIconsBlue" }
      opts.file[".node-version"] = { glyph = "", hl = "MiniIconsGreen" }
      opts.file[".prettierrc"] = { glyph = "", hl = "MiniIconsPurple" }
      opts.file[".yarnrc.yml"] = { glyph = "", hl = "MiniIconsBlue" }
      opts.file["package.json"] = { glyph = "", hl = "MiniIconsGreen" }
      opts.file["tsconfig.json"] = { glyph = "", hl = "MiniIconsAzure" }
      opts.file["tsconfig.build.json"] = { glyph = "", hl = "MiniIconsAzure" }
      opts.file["yarn.lock"] = { glyph = "", hl = "MiniIconsBlue" }
    end,
    config = function(_, opts)
      local icons = require "mini.icons"
      icons.setup(opts)
      icons.mock_nvim_web_devicons()
    end,
  },
  { "echasnovski/mini.pairs", enabled = false, event = "InsertEnter", version = "*", opts = {} },
  { "echasnovski/mini.align", version = "*", keys = { "ga", "gA" }, opts = {} },
  {
    "echasnovski/mini.move",
    keys = {
      { "<", mode = "v" },
      { "J", mode = "v" },
      { "K", mode = "v" },
      { ">", mode = "v" },
      "<M-h>",
      "<M-j>",
      "<M-k>",
      "<M-l>",
    },
    opts = {
      -- Move current line in Visual mode
      mappings = {
        left = "<",
        right = ">",
        down = "J",
        up = "K",

        -- Move current line in Normal mode
        line_left = "<M-h>",
        line_right = "<M-l>",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
  },
  {
    "echasnovski/mini.operators",
    version = "*",
    enabled = true,
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gs", mode = { "n", "x" }, desc = "Sort" },
      "X",
    },
    opts = {
      -- Exchange text regions
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
  {
    "echasnovski/mini.comment",
    version = "*",
    dependencies = {
      "JoosepAlviste/nvim-ts-context-commentstring",
      opts = {
        enable_autocmd = false,
      },
    },
    keys = { { "gc", mode = { "n", "x" } }, { "gcc", mode = { "n", "x" } } },
    opts = {
      options = {
        ignore_blank_line = true,
        custom_commentstring = function()
          return require("ts_context_commentstring").calculate_commentstring() or vim.bo.commentstring
        end,
      },
    },
  },
  {
    "echasnovski/mini.ai",
    version = "*",
    event = "VeryLazy",
    dependencies = {
      { "echasnovski/mini.extra", opts = {} },
    },
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
          n = { "%f[%d]%d+" }, -- digits
          e = { -- Word with case
            { "%u[%l%d]+%f[^%l%d]", "%f[%S][%l%d]+%f[^%l%d]", "%f[%P][%l%d]+%f[^%l%d]", "^[%l%d]+%f[^%l%d]" },
            "^().*()$",
          },
          g = gen_ai_spec.buffer(),
          d = gen_ai_spec.diagnostic(),
          i = gen_ai_spec.indent(),
          L = gen_ai_spec.line(),
          u = ai.gen_spec.function_call(), -- u for "Usage"
          U = ai.gen_spec.function_call { name_pattern = "[%w_]" }, -- without dot in function name
        },
      }
    end,
  },
}
