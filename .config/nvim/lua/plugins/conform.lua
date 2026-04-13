return {
  {
    url = "stevearc/conform.nvim",
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
          ["_"] = { "trim_whitespace" },
        },
      })
      vim.opt.formatexpr = "v:lua.require'conform'.formatexpr()"

      vim.keymap.set("n", "<leader>=", function()
        require("conform").format({ lsp_fallback = true })
      end, { desc = "Format" })
    end,
  },
}
