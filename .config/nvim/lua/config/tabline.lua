local M = {}

local function hl(name)
	return "%#" .. name .. "#"
end

local function escape(value)
	return tostring(value):gsub("%%", "%%%%")
end

local function file_icon(buf_id)
	if _G.MiniIcons == nil then
		return "", nil
	end

	local name = vim.api.nvim_buf_get_name(buf_id)
	if name ~= "" then
		local icon, icon_hl = MiniIcons.get("file", name)
		return icon or "", icon_hl
	end

	local filetype = vim.bo[buf_id].filetype
	if filetype ~= "" then
		local icon, icon_hl = MiniIcons.get("filetype", filetype)
		return icon or "", icon_hl
	end

	return "", nil
end

local function buffer_name(buf_id)
	local name = vim.api.nvim_buf_get_name(buf_id)
	if name ~= "" then
		return vim.fn.fnamemodify(name, ":t")
	end

	local buftype = vim.bo[buf_id].buftype
	if buftype == "quickfix" then
		return "[quickfix]"
	elseif buftype == "nofile" or buftype == "acwrite" then
		return "[scratch]"
	end

	return "[No Name]"
end

local function buffer_group(buf_id)
	local current = buf_id == vim.api.nvim_get_current_buf()
	local visible = vim.fn.bufwinnr(buf_id) > 0
	local modified = vim.bo[buf_id].modified

	if modified and current then
		return "ConfigTablineModifiedCurrent"
	elseif modified and visible then
		return "ConfigTablineModifiedVisible"
	elseif modified then
		return "ConfigTablineModifiedHidden"
	elseif current then
		return "ConfigTablineCurrent"
	elseif visible then
		return "ConfigTablineVisible"
	end

	return "ConfigTablineHidden"
end

local function buffer_item(buf_id)
	local icon, icon_hl = file_icon(buf_id)
	local modified = vim.bo[buf_id].modified and " +" or ""
	local label = escape(buffer_name(buf_id) .. modified)
	local parts = {
		"%" .. buf_id .. "@v:lua.ConfigTablineSwitchBuffer@",
		hl(buffer_group(buf_id)),
		" ",
	}

	if icon ~= "" then
		table.insert(parts, hl(icon_hl or buffer_group(buf_id)))
		local escaped_icon = escape(icon)
		table.insert(parts, escaped_icon)
		table.insert(parts, " ")
		table.insert(parts, hl(buffer_group(buf_id)))
	end

	table.insert(parts, label)
	table.insert(parts, " ")
	table.insert(parts, "%T")

	return table.concat(parts)
end

local function buffers()
	local parts = {}
	for _, buf_id in ipairs(vim.api.nvim_list_bufs()) do
		if vim.bo[buf_id].buflisted then
			table.insert(parts, buffer_item(buf_id))
		end
	end
	return table.concat(parts)
end

local function tabpages()
	local parts = {}
	local current = vim.fn.tabpagenr()

	for index = 1, vim.fn.tabpagenr("$") do
		local group = index == current and "ConfigTablineTabCurrent" or "ConfigTablineTabHidden"
		table.insert(parts, "%" .. index .. "T" .. hl(group) .. " " .. index .. " ")
	end

	table.insert(parts, "%T")
	return table.concat(parts)
end

local function set_highlights()
	local p = vim.g.custom_vague_palette or {
		bg = "#141415",
		inactive_bg = "#1c1c24",
		fg = "#cdcdcd",
		line = "#252530",
		comment = "#606079",
		warning = "#f3be7c",
		parameter = "#bb9dbd",
	}
	local surface = p.inactive_bg or p.bg
	local active = p.line or surface
	local muted = p.comment or p.fg
	local accent = p.parameter or p.fg
	local warning = p.warning or p.delta or p.fg

	local set = function(name, opts)
		vim.api.nvim_set_hl(0, name, opts)
	end

	set("TabLine", { bg = surface, fg = muted })
	set("TabLineSel", { bg = active, fg = p.fg, bold = true })
	set("TabLineFill", { bg = p.bg })
	set("ConfigTablineFill", { bg = p.bg, fg = muted })
	set("ConfigTablineCurrent", { bg = active, fg = p.fg, bold = true })
	set("ConfigTablineVisible", { bg = surface, fg = p.fg })
	set("ConfigTablineHidden", { bg = surface, fg = muted })
	set("ConfigTablineModifiedCurrent", { bg = active, fg = warning, bold = true })
	set("ConfigTablineModifiedVisible", { bg = surface, fg = warning })
	set("ConfigTablineModifiedHidden", { bg = surface, fg = warning })
	set("ConfigTablineTabCurrent", { bg = active, fg = accent, bold = true })
	set("ConfigTablineTabHidden", { bg = surface, fg = muted })
end

function M.render()
	return table.concat({
		buffers(),
		"%#ConfigTablineFill#%=",
		tabpages(),
		"%#ConfigTablineFill#",
	})
end

function M.switch_buffer(buf_id)
	if vim.api.nvim_buf_is_valid(buf_id) then
		vim.api.nvim_set_current_buf(buf_id)
	end
end

function M.setup()
	require("mini.tabline").setup({ tabpage_section = "none" })
	vim.o.tabline = "%!v:lua.require'config.tabline'.render()"
	set_highlights()

	_G.ConfigTablineSwitchBuffer = M.switch_buffer

	vim.api.nvim_create_autocmd("ColorScheme", {
		group = vim.api.nvim_create_augroup("ConfigTabline", { clear = true }),
		desc = "Refresh custom tabline highlights",
		callback = set_highlights,
	})
end

return M
