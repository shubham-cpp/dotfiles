local M = {}

local label = "mark"
local state = { buf = nil, win = nil }

local function path()
	local name = vim.api.nvim_buf_get_name(0)
	return name ~= "" and vim.fn.fnamemodify(name, ":p") or nil
end

local function full_path(name)
	return (vim.fn.fnamemodify(name, ":p"):gsub("/+", "/"):gsub("(.)/$", "%1"))
end

local function cwd()
	return full_path(vim.fn.getcwd())
end

local function has_mark(item)
	return item.labels ~= nil and item.labels[label] == true
end

local function sort_manual_order(mark_items)
	table.sort(mark_items, function(left, right)
		return (left.mark_order or math.huge) < (right.mark_order or math.huge)
	end)
	return mark_items
end

local function items()
	return MiniVisits.list_paths(nil, { filter = has_mark, sort = sort_manual_order })
end

local function valid_win()
	return state.win ~= nil and vim.api.nvim_win_is_valid(state.win)
end

local function valid_buf()
	return state.buf ~= nil and vim.api.nvim_buf_is_valid(state.buf)
end

local function item_at_cursor()
	local index = vim.api.nvim_win_get_cursor(0)[1]
	return items()[index]
end

local function edit(target)
	M.close()
	if target ~= nil then
		vim.cmd.edit(vim.fn.fnameescape(target))
	end
end

local function write_index()
	local ok, err = pcall(MiniVisits.write_index)
	if not ok then
		vim.notify("Could not persist marks: " .. err, vim.log.levels.WARN)
	end
end

local function update_mark_order(target, order)
	local cwd_key = cwd()
	target = full_path(target)

	local index = MiniVisits.get_index()
	index[cwd_key] = index[cwd_key] or {}
	index[cwd_key][target] = index[cwd_key][target] or { count = 0, latest = 0 }
	index[cwd_key][target].mark_order = order
	MiniVisits.set_index(index)
end

local function next_mark_order()
	local max_order = 0
	local index = MiniVisits.get_index()[cwd()] or {}
	for _, item_path in ipairs(items()) do
		local entry = index[item_path]
		if entry ~= nil and type(entry.mark_order) == "number" then
			max_order = math.max(max_order, entry.mark_order)
		end
	end
	return max_order + 1
end

local function set_mark_orders(paths)
	local index = MiniVisits.get_index()
	local cwd_key = cwd()
	index[cwd_key] = index[cwd_key] or {}

	for order, item_path in ipairs(paths) do
		item_path = full_path(item_path)
		index[cwd_key][item_path] = index[cwd_key][item_path] or { count = 0, latest = 0 }
		index[cwd_key][item_path].mark_order = order
	end

	MiniVisits.set_index(index)
end

local function restore_selection(start_line, end_line)
	if not valid_buf() then
		return
	end

	vim.api.nvim_buf_set_mark(state.buf, "<", start_line, 0, {})
	vim.api.nvim_buf_set_mark(state.buf, ">", end_line, 0, {})

	if valid_win() then
		pcall(vim.api.nvim_win_set_cursor, state.win, { start_line, 0 })
		vim.schedule(function()
			if valid_win() then
				vim.api.nvim_set_current_win(state.win)
				pcall(vim.cmd, "normal! gv")
			end
		end)
	end
end

local function render()
	if not valid_buf() then
		return
	end

	local lines = {}
	for index, item_path in ipairs(items()) do
		local name = vim.fn.fnamemodify(item_path, ":.")
		lines[index] = string.format("%d  %s", index, name)
	end
	if #lines == 0 then
		lines = { "No marked files" }
	end

	vim.bo[state.buf].modifiable = true
	vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, lines)
	vim.bo[state.buf].modifiable = false
end

local function map(buf, lhs, rhs, desc)
	vim.keymap.set("n", lhs, rhs, { buffer = buf, desc = desc, nowait = true, silent = true })
end

local function xmap(buf, lhs, rhs, desc)
	vim.keymap.set("x", lhs, rhs, { buffer = buf, desc = desc, nowait = true, silent = true })
end

local function attach_maps(buf)
	for index = 1, 9 do
		map(buf, tostring(index), function()
			M.jump(index)
		end, "Mark: Jump " .. index)
	end

	map(buf, "<CR>", function()
		edit(item_at_cursor())
	end, "Mark: Open")

	map(buf, "d", function()
		local item_path = item_at_cursor()
		if item_path ~= nil then
			M.remove(item_path)
			render()
		end
	end, "Mark: Delete")

	map(buf, "r", render, "Mark: Refresh")
	map(buf, "q", M.close, "Mark: Close")
	map(buf, "<Esc>", M.close, "Mark: Close")

	xmap(buf, "J", function()
		M.move_selection(1)
	end, "Mark: Move Selection Down")

	xmap(buf, "K", function()
		M.move_selection(-1)
	end, "Mark: Move Selection Up")
end

function M.toggle()
	local current = path()
	if current == nil then
		vim.notify("No file to mark", vim.log.levels.WARN)
		return
	end

	if vim.tbl_contains(MiniVisits.list_labels(current), label) then
		MiniVisits.remove_label(label, current)
		update_mark_order(current, nil)
	else
		MiniVisits.add_label(label, current)
		update_mark_order(current, next_mark_order())
	end
	write_index()
	render()
end

function M.remove(target)
	if target ~= nil then
		MiniVisits.remove_label(label, target)
		update_mark_order(target, nil)
		write_index()
	end
end

function M.jump(index)
	edit(items()[index])
end

function M.move_selection(delta, start_line, end_line)
	local mark_paths = items()
	if #mark_paths < 2 then
		return
	end

	start_line = start_line or vim.fn.line("v")
	end_line = end_line or vim.fn.line(".")
	start_line, end_line = math.min(start_line, end_line), math.max(start_line, end_line)
	start_line = math.max(1, math.min(start_line, #mark_paths))
	end_line = math.max(1, math.min(end_line, #mark_paths))

	if delta > 0 and end_line >= #mark_paths then
		return
	end
	if delta < 0 and start_line <= 1 then
		return
	end

	local moved = {}
	for index = start_line, end_line do
		table.insert(moved, mark_paths[index])
	end

	local reordered = {}
	if delta > 0 then
		for index = 1, start_line - 1 do
			table.insert(reordered, mark_paths[index])
		end
		table.insert(reordered, mark_paths[end_line + 1])
		vim.list_extend(reordered, moved)
		for index = end_line + 2, #mark_paths do
			table.insert(reordered, mark_paths[index])
		end
	else
		for index = 1, start_line - 2 do
			table.insert(reordered, mark_paths[index])
		end
		vim.list_extend(reordered, moved)
		table.insert(reordered, mark_paths[start_line - 1])
		for index = end_line + 1, #mark_paths do
			table.insert(reordered, mark_paths[index])
		end
	end

	set_mark_orders(reordered)
	write_index()
	render()

	restore_selection(start_line + delta, end_line + delta)
end

function M.jump_input()
	vim.ui.input({ prompt = "Mark index: " }, function(input)
		local index = tonumber(input)
		if index ~= nil then
			M.jump(index)
		end
	end)
end

function M.close()
	if valid_win() then
		vim.api.nvim_win_close(state.win, true)
	end
	state.win = nil
end

function M.open()
	if valid_win() then
		vim.api.nvim_set_current_win(state.win)
		return
	end

	if not valid_buf() then
		state.buf = vim.api.nvim_create_buf(false, true)
		vim.bo[state.buf].bufhidden = "wipe"
		vim.bo[state.buf].buftype = "nofile"
		vim.bo[state.buf].modifiable = false
		vim.bo[state.buf].swapfile = false
		attach_maps(state.buf)
	end

	local width = math.min(80, math.floor(vim.o.columns * 0.7))
	local height = math.min(12, math.max(3, #items()))
	state.win = vim.api.nvim_open_win(state.buf, true, {
		relative = "editor",
		border = "rounded",
		title = " Marks ",
		title_pos = "center",
		width = width,
		height = height,
		row = math.floor((vim.o.lines - height) / 2),
		col = math.floor((vim.o.columns - width) / 2),
		style = "minimal",
	})
	render()
end

function M.toggle_window()
	if valid_win() then
		M.close()
	else
		M.open()
	end
end

function M.setup(opts)
	opts = opts or {}
	label = opts.label or label
end

return M
