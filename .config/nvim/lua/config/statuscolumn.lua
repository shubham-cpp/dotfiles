local M = {}

local function win()
	return vim.g.statusline_winid ~= nil and vim.g.statusline_winid ~= 0 and vim.g.statusline_winid
		or vim.api.nvim_get_current_win()
end

local function line_number()
	local win_id = win()
	local wo = vim.wo[win_id]
	local width = math.max(wo.numberwidth, #tostring(vim.api.nvim_buf_line_count(vim.api.nvim_win_get_buf(win_id))))

	if vim.v.virtnum ~= 0 then
		return string.rep(" ", width)
	end

	if not wo.number and not wo.relativenumber then
		return ""
	end

	local value = vim.v.lnum
	if wo.relativenumber then
		value = vim.v.relnum == 0 and (wo.number and vim.v.lnum or 0) or vim.v.relnum
	end

	local group = vim.v.relnum == 0 and "CursorLineNr" or "LineNr"
	return "%#" .. group .. "#" .. string.format("%" .. width .. "d", value) .. "%*"
end

function M.render()
	return table.concat({
		"%@v:lua.require'config.statuscolumn'.toggle_fold@",
		"%C",
		"%T",
		line_number(),
		"%s",
	})
end

function M.toggle_fold()
	local pos = vim.fn.getmousepos()
	if pos.winid == 0 or pos.line == 0 then
		return
	end

	vim.api.nvim_set_current_win(pos.winid)
	vim.api.nvim_win_set_cursor(pos.winid, { pos.line, 0 })
	if vim.fn.foldlevel(pos.line) > 0 then
		vim.cmd("normal! za")
	end
end

return M
