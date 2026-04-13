-- Redir: redirect command output to scratch tab
vim.api.nvim_create_user_command("Redir", function(opts)
  local cmd = opts.args
  local output

  if cmd:match("^!") then
    output = vim.split(vim.fn.system(cmd:sub(2)), "\n", { trimempty = true })
  else
    local ok, result = pcall(vim.api.nvim_exec2, cmd, { output = true })
    if not result.output then
      return
    end
    output = ok and vim.split(result.output, "\n", { trimempty = false }) or { result }
  end

  vim.cmd("$tabnew")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
  vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })
  vim.api.nvim_set_option_value("swapfile", false, { buf = buf })
  vim.api.nvim_set_option_value("buflisted", false, { buf = buf })

  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { string.format("Command [[ %s ]] ----- Output ---->", cmd) })
  vim.api.nvim_buf_set_lines(buf, 1, -1, false, output)
end, {
  nargs = 1,
  desc = "Redirect output of a command to scratch tab",
  complete = function(query)
    return vim.fn.getcompletion(query, "command")
  end,
})

-- PrintConfig: inspect vim.pack plugin config
vim.api.nvim_create_user_command("PrintConfig", function(opts)
  local packs = vim.pack.get()
  local args = opts.args
  local function callback(name)
    vim.cmd("Redir lua =vim.pack.get()['" .. name .. "']")
  end
  if args ~= "" then
    callback(args)
    return
  end

  local names = {}
  for _, p in ipairs(packs) do
    table.insert(names, p.name)
  end
  vim.ui.select(names, { prompt = "Select plugin to inspect" }, function(item)
    if item then
      callback(item)
    end
  end)
end, {
  desc = "Print vim.pack plugin config",
  nargs = "?",
  complete = function(prefix)
    local names = {}
    for _, p in ipairs(vim.pack.get()) do
      if prefix == "" or p.name:match(prefix) then
        table.insert(names, p.name)
      end
    end
    return names
  end,
})

-- RunMake: save + set compiler + run :Make
local rtps = vim.api.nvim_list_runtime_paths()
local all_comps = {}
for _, p in ipairs(rtps) do
  for _, f in ipairs(vim.fn.globpath(p, "compiler/*.vim", 0, 1)) do
    table.insert(all_comps, vim.fn.fnamemodify(f, ":t:r"))
  end
end

vim.api.nvim_create_user_command("RunMake", function(opts)
  vim.cmd("update")
  local compiler = opts.fargs[1]
  vim.cmd("compiler " .. compiler)
  if #opts.fargs > 1 then
    vim.cmd("Make " .. table.concat(vim.list_slice(opts.fargs, 2), " "))
  else
    vim.cmd("Make")
  end
end, {
  nargs = "+",
  desc = "Set compiler and run Make",
  complete = function(arg_lead, cmd_line)
    local parts = vim.split(cmd_line, "%s+")
    if #parts == 1 or (#parts == 2 and arg_lead == parts[2]) then
      return vim.tbl_filter(function(c)
        return vim.startswith(c, arg_lead)
      end, all_comps)
    else
      return vim.fn.getcompletion(arg_lead, "file")
    end
  end,
})

-- CreateFileInDir: pick directory and create file
local function list_subdirs(cwd)
  local has_fd = vim.fn.executable("fd") == 1 or vim.fn.executable("fdfind") == 1
  if not has_fd then
    vim.notify("You need to install fd", vim.log.levels.WARN)
    return nil
  end
  local bin = vim.fn.executable("fd") == 1 and "fd" or "fdfind"
  local res = vim.system({ bin, "-t", "d" }, { cwd = cwd, text = true }):wait()
  local dirs = {}
  if res and res.code == 0 and res.stdout then
    for line in res.stdout:gmatch("[^\r\n]+") do
      if line ~= "." and line ~= "" then
        table.insert(dirs, line)
      end
    end
  end
  table.sort(dirs)
  table.insert(dirs, 1, ".")
  return dirs
end

vim.api.nvim_create_user_command("CreateFileInDir", function()
  local cwd = vim.uv.cwd()
  if not cwd then
    return
  end
  local dirs = list_subdirs(cwd)
  if not dirs or next(dirs) == nil then
    return
  end
  vim.ui.select(dirs, {
    prompt = "Select directory",
    format_item = function(item)
      if item == "." then
        return "[.] current directory"
      end
      return item
    end,
  }, function(choice)
    if not choice then
      return
    end
    local base_dir = choice == "." and cwd or vim.fs.joinpath(cwd, choice)
    vim.ui.input({ prompt = "New file path (relative to " .. base_dir .. "):" }, function(input)
      if not input then
        return
      end
      input = input:gsub("^%./", ""):gsub("/+$", "")
      if input == "" then
        vim.notify("No file name provided", vim.log.levels.WARN)
        return
      end
      local full_path = input:sub(1, 1) == "/" and input or vim.fs.joinpath(base_dir, input)
      local st = vim.uv.fs_stat(full_path)
      if st and st.type == "directory" then
        vim.notify("Path is a directory: " .. full_path, vim.log.levels.WARN)
        return
      end
      if not st then
        local parent = vim.fs.dirname(full_path)
        if parent and parent ~= "" then
          vim.fn.mkdir(parent, "p")
        end
        local fd = vim.uv.fs_open(full_path, "w", 420)
        if not fd then
          vim.notify("Failed to create file: " .. full_path, vim.log.levels.WARN)
          return
        end
        vim.uv.fs_close(fd)
        vim.cmd.edit(vim.fn.fnameescape(full_path))
        vim.notify("Opened: " .. full_path, vim.log.levels.INFO)
      end
    end)
  end)
end, { desc = "Pick directory and create a file" })
