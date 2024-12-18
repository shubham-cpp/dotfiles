local prettier = { "prettierd", "prettier", stop_after_first = true }

---@param bufnr integer
---@param ... string
---@return string
local function first(bufnr, ...)
  local conform = require "conform"
  for i = 1, select("#", ...) do
    local formatter = select(i, ...)
    if conform.get_formatter_info(formatter, bufnr).available then return formatter end
  end
  return select(1, ...)
end

local function prettier_eslint(bufnr) return { first(bufnr, "prettierd", "prettier"), "eslint_d" } end

---@type LazySpec
return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      css = prettier,
      html = prettier,
      jsonc = prettier,
      json = prettier,
      json5 = prettier,
      less = prettier,
      markdown = prettier,
      sass = prettier,
      scss = prettier,
      astro = prettier_eslint,
      javascript = prettier_eslint,
      javascriptreact = prettier_eslint,
      typescript = prettier_eslint,
      typescriptreact = prettier_eslint,
      svelte = prettier_eslint,
      vue = prettier_eslint,
    },
  },
}
