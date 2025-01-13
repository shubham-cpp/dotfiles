local M = {}

M.unique_append_table = function(list, values)
  local appendValues = {}
  local l = list or {}
  if not values or next(values) == nil then
    return l
  end
  for _, value in ipairs(values) do
    if not vim.tbl_contains(l, value) then
      table.insert(appendValues, value)
    end
  end
  return vim.list_extend(l, appendValues)
end

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

function M.get_lsp_capabilities(override)
  local ok_cmp, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
  local ok_blink, blink = pcall(require, 'blink.cmp')
  local capabilities
  if ok_cmp then
    capabilities = cmp_nvim_lsp.default_capabilities(override)
  elseif ok_blink then
    capabilities = blink.get_lsp_capabilities(override)
  end
  return capabilities
end

return M
