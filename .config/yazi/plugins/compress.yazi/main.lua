--- @since 25.12.29

local is_windows = ya.target_family() == "windows"

local function notify(message, level)
	ya.notify({
		title = "Archive",
		content = message,
		level = level,
		timeout = 5,
	})
end

local function extension_pattern(ext)
	return "%." .. ext:gsub("%.", "%%.") .. "$"
end

local function is_valid_filename(name)
	name = name:match("^%s*(.-)%s*$")
	if name == "" then
		return false
	end

	if is_windows then
		return not name:find('[<>:"/\\|%?%*]')
	end

	return not name:find("/") and not name:find("%z")
end

local function is_command_available(cmd)
	local stat_cmd
	if is_windows then
		stat_cmd = string.format("where %s > nul 2>&1", cmd)
	else
		stat_cmd = string.format("command -v %s >/dev/null 2>&1", cmd)
	end

	return os.execute(stat_cmd)
end

local function find_command_name(cmds)
	if type(cmds) == "string" then
		return is_command_available(cmds) and cmds or nil
	end

	for _, cmd in ipairs(cmds) do
		if is_command_available(cmd) then
			return cmd
		end
	end
end

local function combine_url(path, file)
	return tostring(Url(path):join(Url(file)))
end

local function copy_table(items)
	local out = {}
	for i, item in ipairs(items or {}) do
		out[i] = item
	end
	return out
end

local selected_or_hovered = ya.sync(function()
	local tab = cx.active
	local names, path_fnames = {}, {}

	for _, u in pairs(tab.selected) do
		local parent = tostring(u.parent)
		local name = tostring(u.name)
		path_fnames[parent] = path_fnames[parent] or {}
		path_fnames[parent][#path_fnames[parent] + 1] = name
		names[#names + 1] = name
	end

	if #names == 0 and tab.current.hovered then
		local hovered = tab.current.hovered
		local parent = tostring(hovered.url.parent)
		path_fnames[parent] = { tostring(hovered.name) }
		names[1] = tostring(hovered.name)
	end

	return path_fnames, names, tostring(tab.current.cwd)
end)

local function cleanup_temp_dir(temp_dir)
	local ok, err = fs.remove("dir_all", Url(temp_dir))
	if not ok then
		notify(string.format("Failed to clean up %s: %s", ya.quote(temp_dir), tostring(err)), "warn")
	end
end

local archive_commands = {
	{
		pattern = "%.zip$",
		variants = {
			{ command = "zip", args = { "-r" } },
			{ command = { "7z", "7zz", "7za" }, args = { "a", "-tzip" } },
		},
	},
	{
		pattern = "%.rar$",
		variants = {
			{ command = "rar", args = { "a" } },
		},
	},
	{
		pattern = "%.7z$",
		variants = {
			{ command = { "7z", "7zz", "7za" }, args = { "a" } },
		},
	},
	{
		pattern = "%.tar%.gz$",
		tar_name = function(name)
			return name:match("^(.*)%.gz$")
		end,
		variants = {
			{ command = { "tar", "bsdtar" }, args = { "rpf" }, compress = "gzip" },
		},
	},
	{
		pattern = "%.tar%.xz$",
		tar_name = function(name)
			return name:match("^(.*)%.xz$")
		end,
		variants = {
			{ command = { "tar", "bsdtar" }, args = { "rpf" }, compress = "xz" },
		},
	},
	{
		pattern = "%.tar%.bz2$",
		tar_name = function(name)
			return name:match("^(.*)%.bz2$")
		end,
		variants = {
			{ command = { "tar", "bsdtar" }, args = { "rpf" }, compress = "bzip2" },
		},
	},
	{
		pattern = "%.tar%.zst$",
		tar_name = function(name)
			return name:match("^(.*)%.zst$")
		end,
		variants = {
			{ command = { "tar", "bsdtar" }, args = { "rpf" }, compress = "zstd", compress_args = { "--rm" } },
		},
	},
	{
		pattern = "%.tar$",
		variants = {
			{ command = { "tar", "bsdtar" }, args = { "rpf" } },
		},
	},
}

local function find_archive_command(output_name)
	for _, archive in ipairs(archive_commands) do
		if output_name:match(archive.pattern) then
			for _, variant in ipairs(archive.variants) do
				local cmd = find_command_name(variant.command)
				if cmd and (not variant.compress or is_command_available(variant.compress)) then
					return {
						cmd = cmd,
						args = copy_table(variant.args),
						compress = variant.compress,
						compress_args = copy_table(variant.compress_args),
						tar_name = archive.tar_name,
					}
				end
			end
			return nil
		end
	end

	return false
end

local function default_extension_from_job(job)
	local default = "zip"
	for _, arg in ipairs(job.args or {}) do
		if arg:match("^[%w%.]+$") then
			local pattern = extension_pattern(arg)
			for _, archive in ipairs(archive_commands) do
				if archive.pattern == pattern then
					default = arg
					break
				end
			end
		end
	end
	return default
end

return {
	entry = function(_, job)
		ya.emit("escape", { visual = true })

		local path_fnames, names, output_dir = selected_or_hovered()
		if #names == 0 then
			notify("No files selected or hovered", "warn")
			return
		end

		local default_extension = default_extension_from_job(job)
		local default_name = #names == 1 and names[1] or Url(output_dir).name
		local output_name, event = ya.input({
			title = "Create archive:",
			value = string.format("%s.%s", default_name, default_extension),
			pos = { "top-center", y = 3, w = 40 },
		})
		if event ~= 1 then
			return
		end

		output_name = output_name:match("^%s*(.-)%s*$")
		if output_name == "" then
			output_name = string.format("%s.%s", default_name, default_extension)
		elseif not output_name:match("%.%w+$") then
			output_name = string.format("%s.%s", output_name, default_extension)
		end

		if not is_valid_filename(output_name) then
			notify("Invalid archive filename", "error")
			return
		end

		local archive = find_archive_command(output_name)
		if archive == false then
			notify("Unsupported file extension", "error")
			return
		elseif not archive then
			notify("No archive program found for this extension", "error")
			return
		end

		local temp_dir = tostring(fs.unique_name(Url(combine_url(output_dir, ".tmp_compress"))))
		local ok, err = fs.create("dir_all", Url(temp_dir))
		if not ok then
			notify(string.format("Failed to create temp directory: %s", tostring(err)), "error")
			return
		end

		local original_name = output_name
		local archive_name = archive.tar_name and archive.tar_name(output_name) or output_name
		local temp_output = combine_url(temp_dir, archive_name)

		for path, filenames in pairs(path_fnames) do
			local status, cmd_err = Command(archive.cmd):arg(archive.args):arg(temp_output):arg(filenames):cwd(path):spawn():wait()
			if not status or not status.success then
				notify(
					string.format("Failed to create %s with %s: %s", ya.quote(archive_name), archive.cmd, tostring(cmd_err)),
					"error"
				)
				cleanup_temp_dir(temp_dir)
				return
			end
		end

		if archive.compress then
			local status, cmd_err = Command(archive.compress):arg(archive.compress_args):arg(temp_output):spawn():wait()
			if not status or not status.success then
				notify(
					string.format("Failed to compress %s with %s: %s", ya.quote(archive_name), archive.compress, tostring(cmd_err)),
					"error"
				)
				cleanup_temp_dir(temp_dir)
				return
			end
		end

		local final_output = tostring(fs.unique_name(Url(combine_url(output_dir, original_name))))
		local temp_final = combine_url(temp_dir, original_name)
		local moved, move_err = fs.rename(Url(temp_final), Url(final_output))
		if not moved then
			if move_err and move_err.kind == "CrossesDevices" then
				moved, move_err = fs.copy(Url(temp_final), Url(final_output))
			end
			if not moved then
				notify(string.format("Failed to move archive: %s", move_err and tostring(move_err.kind) or "unknown"), "error")
				cleanup_temp_dir(temp_dir)
				return
			end
		end

		cleanup_temp_dir(temp_dir)
		notify(string.format("Created archive: %s", ya.quote(Url(final_output).name)), "info")
	end,
}
