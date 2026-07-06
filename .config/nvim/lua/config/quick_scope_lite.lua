local M = {}

M.ns_id = vim.api.nvim_create_namespace "quick_scope_lite"

local uv = vim.uv or vim.loop

local config = {
	timeout = 1000,
	max_chars = 1000,
	primary_hl = "QuickScopePrimary",
	secondary_hl = "QuickScopeSecondary",
	disabled_buftypes = {
		acwrite = true,
		help = true,
		nofile = true,
		prompt = true,
		quickfix = true,
		terminal = true,
	},
	disabled_filetypes = {
		checkhealth = true,
		dashboard = true,
		git = true,
		help = true,
		lspinfo = true,
		man = true,
		qf = true,
		snacks_dashboard = true,
		startify = true,
	},
}

local active_buffers = {}
local timer

local function stop_timer()
	if timer ~= nil then
		timer:stop()
	end
end

function M.clear()
	stop_timer()

	for bufnr in pairs(active_buffers) do
		if vim.api.nvim_buf_is_valid(bufnr) then
			vim.api.nvim_buf_clear_namespace(bufnr, M.ns_id, 0, -1)
		end
	end

	active_buffers = {}
end

local function is_disabled(bufnr)
	if vim.b[bufnr].quick_scope_lite_disable or vim.b[bufnr].is_bigfile then
		return true
	end

	if config.disabled_buftypes[vim.bo[bufnr].buftype] then
		return true
	end

	if config.disabled_filetypes[vim.bo[bufnr].filetype] then
		return true
	end

	return false
end

local function is_targetable(byte)
	return (byte >= 48 and byte <= 57) or (byte >= 65 and byte <= 90) or (byte >= 97 and byte <= 122)
end

local function is_keyword(byte)
	return is_targetable(byte) or byte == 95
end

local function add_mark(bufnr, row, col, hl_group, priority)
	vim.api.nvim_buf_set_extmark(bufnr, M.ns_id, row, col, {
		end_col = col + 1,
		hl_group = hl_group,
		priority = priority,
		strict = false,
	})
end

local function flush_word(bufnr, row, primary_col, secondary_col)
	if primary_col then
		add_mark(bufnr, row, primary_col, config.primary_hl, 210)
	elseif secondary_col then
		add_mark(bufnr, row, secondary_col, config.secondary_hl, 200)
	end
end

local function schedule_clear()
	if config.timeout <= 0 then
		return
	end

	timer = timer or uv.new_timer()
	stop_timer()
	timer:start(config.timeout, 0, vim.schedule_wrap(M.clear))
end

function M.show(key)
	local bufnr = vim.api.nvim_get_current_buf()
	M.clear()

	if is_disabled(bufnr) then
		return
	end

	local line = vim.api.nvim_get_current_line()
	if line == "" or #line > config.max_chars then
		return
	end

	local cursor = vim.api.nvim_win_get_cursor(0)
	local row = cursor[1] - 1
	local cursor_col = cursor[2]
	local forward = key == "f" or key == "t"
	local count = vim.v.count1
	local occurrences = {}
	local first_word = true
	local primary_col, secondary_col
	local start_col, stop_col, step

	if forward then
		start_col, stop_col, step = cursor_col + 1, #line - 1, 1
	else
		start_col, stop_col, step = cursor_col - 1, 0, -1
	end

	if forward and start_col > stop_col then
		return
	end
	if not forward and start_col < stop_col then
		return
	end

	for col = start_col, stop_col, step do
		local byte = line:byte(col + 1)
		if byte == nil or not is_keyword(byte) then
			if not first_word then
				flush_word(bufnr, row, primary_col, secondary_col)
			end

			first_word = false
			primary_col, secondary_col = nil, nil
		elseif is_targetable(byte) then
			occurrences[byte] = (occurrences[byte] or 0) + 1

			if not first_word then
				if occurrences[byte] == count and (not forward or primary_col == nil) then
					primary_col = col
				elseif occurrences[byte] == count + 1 and (not forward or secondary_col == nil) then
					secondary_col = col
				end
			end
		end
	end

	flush_word(bufnr, row, primary_col, secondary_col)
	active_buffers[bufnr] = true
	schedule_clear()
end

local function map_motion(key)
	vim.keymap.set({ "n", "x", "o" }, key, function()
		M.show(key)
		return key
	end, {
		desc = "QuickScope-lite " .. key,
		expr = true,
		silent = false,
	})
end

function M.setup(opts)
	config = vim.tbl_deep_extend("force", config, opts or {})

	vim.api.nvim_create_autocmd({ "CursorMoved", "ModeChanged", "BufLeave", "InsertEnter", "WinScrolled" }, {
		group = vim.api.nvim_create_augroup("QuickScopeLite", { clear = true }),
		desc = "Clear QuickScope-lite line highlights",
		callback = M.clear,
	})

	for _, key in ipairs { "f", "F", "t", "T" } do
		map_motion(key)
	end
end

return M
