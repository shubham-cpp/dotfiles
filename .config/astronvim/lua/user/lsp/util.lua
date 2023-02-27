local M = {}

M.bun_path = function()
  local bun = os.getenv 'BUN_INSTALL'
  local xdg_data = os.getenv 'XDG_DATA_HOME' or os.getenv 'HOME' .. '/.local/share'
  if not bun then
    return xdg_data .. '/bun/bin'
  end
  return bun .. '/bin'
end

return M
