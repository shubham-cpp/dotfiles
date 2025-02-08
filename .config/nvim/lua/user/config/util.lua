local M = {}

--- A wrapper around `age.nvim` to get the credentials from the identity file.
---@param secret_file string The secret file to read the credentials from(`secret_file` should be in location `$HOME/.config/age/`).
---@return string|nil secret The credentials from the identity file.
M.get_age_credentials = function(secret_file)
  local identity_file = vim.fn.expand '$HOME/.config/age/identity.txt'

  if 0 == vim.fn.filereadable(identity_file) then
    return nil
  end

  local secret = vim.fn.expand('$HOME/.config/age/' .. secret_file)
  return require('age').get(secret, identity_file)
end

---@param override? lsp.ClientCapabilities
---@return lsp.ClientCapabilities
function M.get_lsp_capabilities(override)
  local ok_cmp, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
  local ok_blink, blink = pcall(require, 'blink.cmp')
  local capabilities
  if ok_cmp then
    capabilities = cmp_nvim_lsp.default_capabilities(override or {})
  elseif ok_blink then
    capabilities = blink.get_lsp_capabilities(override)
  end
  return capabilities
end

---Remove duplicates from table
---@param tbl table
---@return table
function M.dedup(tbl)
  local unique = {}
  local result = {}

  for _, value in ipairs(tbl) do
    if not unique[value] then
      table.insert(result, value)
      unique[value] = true
    end
  end

  return result
end

---A little helper for conform.nvim, to select configure formatters the old way
---[Github Source](https://github.com/stevearc/conform.nvim/blob/master/doc/recipes.md#run-the-first-available-formatter-followed-by-more-formatters)
---Example:
---```lua
---require('conform').setup({
---  formatters_by_ft = {
---     go = function(bufnr)
---       return { 'goimports', first(bufnr, 'gofumpt', 'gofmt') }
---     end,
---     typescript = function(bufnr)
---       return { first(bufnr, 'prettierd', 'prettier'), 'eslint_d'  }
---     end,
---   }
--- })
---```
---@param bufnr integer
---@param ... string
---@return string
function M.first_formatter(bufnr, ...)
  local conform = require 'conform'
  for i = 1, select('#', ...) do
    local formatter = select(i, ...)
    if conform.get_formatter_info(formatter, bufnr).available then
      return formatter
    end
  end
  return select(1, ...)
end

return M
