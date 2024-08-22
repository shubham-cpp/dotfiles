---@type LazySpec
return {
  {
    "williamboman/mason.nvim",
    opts = {
      PATH = "append",
      ensure_installed = {
        "eslint_d",
        "shellcheck",
        "golangci-lint",
      },
    },
  },

  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        fish = { "fish" },
        bash = { "shellcheck" },
        sh = { "shellcheck" },
        svelte = { "eslint_d" },
        javascript = { "eslint_d" },
        javascriptreact = { "eslint_d" },
        typescript = { "eslint_d" },
        typescriptreact = { "eslint_d" },
        vue = { "eslint_d" },
        astro = { "eslint_d" },
        go = { "golangcilint" },
        php = { "phpcs" },
      },
      linters = {
        -- -- Example of using selene only when a selene.toml file is present
        eslint_d = {
          condition = function(ctx)
            return vim.fs.find({
              ".eslintrc",
              ".eslintrc.js",
              ".eslintrc.cjs",
              ".eslintrc.yaml",
              ".eslintrc.yml",
              ".eslintrc.json",
              "eslint.config.js",
              "eslint.config.mjs",
              "eslint.config.cjs",
            }, { path = ctx.filename, upward = true })[1]
          end,
        },
      },
    },
  },
}
