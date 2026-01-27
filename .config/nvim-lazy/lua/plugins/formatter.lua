local prettier = { "prettierd", "prettier", stop_after_first = true }
---@param bufnr integer
---@param ... string
---@return string
local function first(bufnr, ...)
  local conform = require("conform")
  for i = 1, select("#", ...) do
    local formatter = select(i, ...)
    if conform.get_formatter_info(formatter, bufnr).available then
      return formatter
    end
  end
  return select(1, ...)
end

local function prettier_eslint(bufnr)
  return { first(bufnr, "prettierd", "prettier"), "eslint_d" }
  -- return { first(bufnr, "prettierd", "prettier") }
end
---@type LazySpec
return {
  "stevearc/conform.nvim",
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
      css = prettier,
      scss = prettier,
      less = prettier,
      html = prettier,
      json = prettier,
      jsonc = prettier,
      yaml = prettier,
      markdown = prettier,
      handlebars = prettier,
      ["markdown.mdx"] = prettier,
      ["graphql"] = prettier_eslint,
      ["javascript"] = prettier_eslint,
      ["javascriptreact"] = prettier_eslint,
      ["typescript"] = prettier_eslint,
      ["typescriptreact"] = prettier_eslint,
      ["vue"] = prettier_eslint,
      ["svelte"] = prettier_eslint,
      ["astro"] = prettier_eslint,
      fish = { "fish_indent" },
      nim = { "nimpretty" },
      zig = { "zigfmt" },
      bash = { "shfmt" },
      php = { "phpcbf", "php_cs_fixer" },
      blade = { "blade-formatter" },
      go = function(bufnr)
        return { "goimports", first(bufnr, "gofumpt", "gofmt") }
      end,
      python = { "ruff_format", "ruff_fix", "ruff_organize_imports" },
    },
    formatters = {
      prettierd = {
        condition = function()
          return not (vim.b.disable_prettier or vim.g.disabled_prettier)
        end,
      },
      prettier = {
        condition = function()
          return not (vim.b.disable_prettier or vim.g.disabled_prettier)
        end,
      },
    },
  },
}
