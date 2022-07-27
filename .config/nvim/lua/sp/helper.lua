local M = {}

---An alias to nvim_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string|function the command to execute
---@param opts ?table contains options like noremap,silent
---@see nvim_set_keymap
function M.map(mode, lhs, rhs, opts)
	local opts = opts == nil and { silent = true, noremap = true } or opts
	vim.keymap.set(mode, lhs, rhs, opts)
end

---An alias to nvim_buf_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string|function the command to execute
---@param opts ?table contains options like noremap,silent
---@param bufnr ?number buffer id
---@see nvim_buf_set_keymap
function M.bmap(mode, lhs, rhs, opts, bufnr)
	local opts = opts == nil and { silent = true, noremap = true } or opts
	opts.buffer = bufnr or true
	vim.keymap.set(mode, lhs, rhs, opts)
end

---Wrapper function to print lua table
---@param value table Table to print
function _G.p(value)
	print(vim.inspect(value))
end

return M
