local prettier = { "prettierd", "prettier" }
---@type LazySpec
return {
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.PATH = "append"
      opts.ensure_installed = vim.tbl_extend("force", opts.ensure_installed, {
        "prettierd",
        "prettier",
        "shfmt",
      })
    end,
  },
  {
    "stevearc/conform.nvim",
    optional = true,
    keys = {
      {
        -- Customize or remove this keymap to your liking
        "<leader>=",
        function()
          require("conform").format({ async = true, lsp_fallback = true })
        end,
        mode = "",
        desc = "Format buffer(c)",
      },
    },
    opts = {
      formatters_by_ft = {
        ["javascript"] = { prettier, "eslint_d" },
        ["javascriptreact"] = { prettier, "eslint_d" },
        ["typescript"] = { prettier, "eslint_d" },
        ["typescriptreact"] = { prettier, "eslint_d" },
        ["vue"] = { prettier, "eslint_d" },
        ["svelte"] = { prettier, "eslint_d" },
        ["astro"] = { prettier, "eslint_d" },
        ["css"] = { prettier },
        ["scss"] = { prettier },
        ["less"] = { prettier },
        ["html"] = { prettier },
        ["json"] = { prettier },
        ["jsonc"] = { prettier },
        ["yaml"] = { prettier },
        ["markdown"] = { prettier },
        ["markdown.mdx"] = { prettier },
        ["graphql"] = { prettier, "eslint_d" },
        ["handlebars"] = { prettier },
        fish = { "fish_indent" },
        nim = { "nimpretty" },
        zig = { "zigfmt" },
        bash = { "shfmt" },
        php = { "phpcbf", "php_cs_fixer" },
        blade = { "blade-formatter" },
      },
    },
  },
}
