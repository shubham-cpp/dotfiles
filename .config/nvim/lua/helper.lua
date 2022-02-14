local M = {}

---An alias to nvim_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string the command to execute
---@param opts table contains options like noremap,silent
---@see nvim_set_keymap
function M.map(mode, lhs, rhs, opts)
	local opts = opts or { silent = true }
	vim.keymap.set(mode, lhs, rhs, opts)
	-- vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
end

---An alias to nvim_buf_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string the command to execute
---@param opts table contains options like noremap,silent
---@param bufnr number buffer id
---@see nvim_buf_set_keymap
function M.bmap(mode, lhs, rhs, opts, bufnr)
	local opts = opts or { silent = true }
	opts.buffer = bufnr or true
	vim.keymap.set(mode, lhs, rhs, opts)
end

---An alias to nvim_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string the command to execute
---@param opts table contains options like noremap,silent
---@see nvim_set_keymap
function M.old_map(mode, lhs, rhs, opts)
	local opts = opts or { noremap = true, silent = true }
	vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
end

---An alias to nvim_buf_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string the command to execute
---@param opts table contains options like noremap,silent
---@param bufnr number buffer id
---@see nvim_buf_set_keymap
function M.old_bmap(mode, lhs, rhs, opts, bufnr)
	local bno = bufnr or 0
	local opts = opts or { noremap = true, silent = true }
	vim.api.nvim_buf_set_keymap(bno, mode, lhs, rhs, opts)
end

---Wrapper function to print lua table
---@param value table Table to print
function _G.p(value)
	print(vim.inspect(value))
end

return M
