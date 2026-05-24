local function disable_prettier_condition()
  return not (vim.b.disable_prettier or vim.g.disabled_prettier)
end
return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  keys = {
    {
      "<leader>=",
      function()
        require("conform").format({ async = true, lsp_format = "fallback" })
      end,
      mode = { "n", "v" },
      desc = "Format(Conform)",
    },
  },
  init = function()
    vim.opt.formatexpr = "v:lua.require'conform'.formatexpr()"
  end,
  config = function()
    local prettier = { "oxfmt", "prettierd", "prettier", stop_after_first = true }

    require("conform").setup({
      default_format_opts = {
        lsp_format = "fallback",
      },
      format_on_save = function(bufnr)
        if vim.b[bufnr].bigfile then
          return
        end
        if vim.b[bufnr].disable_auto_format or vim.g.disable_auto_format then
          return
        end
        return { timeout_ms = 500 }
      end,
      formatters_by_ft = {
        lua = { "stylua" },
        sh = { "shfmt" },
        fish = { "fish_indent" },
        go = { "goimports", "gofumpt" },
        python = { "ruff_fix", "ruff_organize_imports", "ruff_format" },
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
      formatters = {
        prettierd = { condition = disable_prettier_condition },
        prettier = { condition = disable_prettier_condition },
        oxfmt = { condition = disable_prettier_condition },
      },
    })
  end,
}
