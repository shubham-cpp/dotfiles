local M = {}

M.bun_path = function()
  local bun = os.getenv 'BUN_INSTALL'
  local xdg_data = os.getenv 'XDG_DATA_HOME' or os.getenv 'HOME' .. '/.local/share'
  if not bun then return xdg_data .. '/bun/bin' end
  return bun .. '/bin'
end

M.map = function(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts and next(opts) ~= nil then vim.tbl_extend('force', options, opts) end
  vim.keymap.set(mode, lhs, rhs, options)
end

return M
