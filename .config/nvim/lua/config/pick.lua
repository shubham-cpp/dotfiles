local M = {}

local ns_id = vim.api.nvim_create_namespace("ConfigPick")

local function split_path(path)
	local from, to = path:find(".*[/\\]")
	if from == nil then
		return path, ""
	end

	local dirname = path:sub(from, to - 1)
	local basename = path:sub(to + 1)
	return basename ~= "" and basename or path, dirname
end

local function path_text(path)
	if type(path) == "table" then
		return path.path or path.text or ""
	end
	return tostring(path or "")
end

local function display_parts(path)
	local basename, dirname = split_path(path)
	if dirname == "" then
		return basename, nil
	end
	return basename .. " " .. dirname, #basename + 1
end

function M.filename_first(path)
	return display_parts(path)
end

function M.show_filename_first(buf_id, items, query)
	local display_items, dim_from = {}, {}

	for i, item in ipairs(items) do
		local path = path_text(item)
		local text, dir_col = display_parts(path)
		display_items[i] = { text = text, path = path }
		dim_from[i] = dir_col
	end

	MiniPick.default_show(buf_id, display_items, query, { show_icons = true })

	vim.api.nvim_buf_clear_namespace(buf_id, ns_id, 0, -1)
	for i, col in ipairs(dim_from) do
		if col ~= nil then
			local line = vim.api.nvim_buf_get_lines(buf_id, i - 1, i, false)[1] or ""
			local start = line:find(display_items[i].text, 1, true)
			if start ~= nil then
				vim.api.nvim_buf_set_extmark(buf_id, ns_id, i - 1, start + col - 1, {
					end_col = #line,
					hl_group = "Comment",
					hl_mode = "combine",
					priority = 203,
				})
			end
		end
	end
end

function M.source_opts(source)
	return { source = vim.tbl_extend("force", { show = M.show_filename_first }, source or {}) }
end

return M
