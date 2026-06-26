local M = {}

M.gh = function(x)
	return "https://github.com/" .. x
end

M.ignored_path_fragments = {
	"/node_modules/",
	"/.git/",
	"/dist/",
	"/build/",
	"/coverage/",
	"/.next/",
	"/.svelte-kit/",
}
M.buf_path = function(bufnr)
	local name = vim.api.nvim_buf_get_name(bufnr)
	if name == "" then
		return ""
	end
	return vim.fs.normalize(name)
end

M.is_ignored = function(path)
	for _, fragment in ipairs(M.ignored_path_fragments) do
		if path:find(fragment, 1, true) then
			return true
		end
	end
	return false
end

---@param bufnr number
---@param check_readonly boolean?
M.should_apply = function(bufnr, check_readonly)
	if not vim.api.nvim_buf_is_valid(bufnr) then
		return false
	end

	if vim.bo[bufnr].buftype ~= "" then
		return false
	end

	if check_readonly and (not vim.bo[bufnr].modifiable or vim.bo[bufnr].readonly) then
		return false
	end

	local path = M.buf_path(bufnr)
	if path == "" then
		return false
	end

	if M.is_ignored(path) then
		return false
	end

	return true
end

M.get_keys = function(tbl)
	local k = {}

	for key, val in pairs(tbl) do
		table.insert(k, type(val) == "table" and key or val)
	end

	return k
end

return M
