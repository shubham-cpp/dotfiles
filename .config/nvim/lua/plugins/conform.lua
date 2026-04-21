return {
  "stevearc/conform.nvim",
  keys = {
    {
      "<leader>=",
      function()
        require("conform").format({ lsp_format = "fallback" })
      end,
      desc = "Format(Conform)",
    },
  },
  config = function()
    local prettier = { "oxfmt", "prettierd", "prettier", stop_after_first = true }

    require("conform").setup({
      formatters_by_ft = {
        lua = { "stylua" },
        sh = { "shfmt" },
        fish = { "fish_indent" },
        go = { "goimports", "gofumpt" },
        python = { "ruff_fix", "ruff_format", "ruff_organize_imports" },
        javascript = prettier,
        typescript = prettier,
        javascriptreact = prettier,
        typescriptreact = prettier,
        css = prettier,
        html = prettier,
        json = prettier,
        yaml = prettier,
        markdown = prettier,
        rust = { "rustfmt" },
        ["_"] = { "trim_whitespace" },
      },
    })
    vim.opt.formatexpr = "v:lua.require'conform'.formatexpr()"
  end,
}
