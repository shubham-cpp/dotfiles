local M = {}

--- A wrapper around `age.nvim` to get the credentials from the identity file.
---@param secret_file string The secret file to read the credentials from(`secret_file` should be in location `$HOME/.config/age/`).
---@return string|nil secret The credentials from the identity file.
function M.get_age_credentials(secret_file)
  if not vim.fn.filereadable("$HOME/.config/age/identity.txt") then
    return nil
  end
  local identity = vim.fn.expand("$HOME/.config/age/identity.txt")
  local secret = vim.fn.expand("$HOME/.config/age/" .. secret_file)
  return require("age").get(secret, identity)
end

return M
