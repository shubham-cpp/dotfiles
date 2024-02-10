local M = {}

M.xdg_data = function()
	local xdg_data_home = os.getenv("XDG_DATA_HOME")
	if not xdg_data_home then
		xdg_data_home = os.getenv("HOME") .. "/.local/share"
	end
	return xdg_data_home
end

M.xdg_data_bin = function()
	return os.getenv("HOME") .. "/.local/bin"
end

M.bun_path = function()
	local bun = os.getenv("BUN_INSTALL")
	local xdg_data = os.getenv("XDG_DATA_HOME") or os.getenv("HOME") .. "/.local/share"
	if not bun then
		return xdg_data .. "/bun/bin"
	end
	return bun .. "/bin"
end

---Will wrap the `command` in "<cmd><cr>"
---@param command string
M.cmd = function(command)
	return "<cmd>" .. command .. "<cr>"
end

return M
