local uv = vim.loop
---Get a list of all the directories in lua table format
---@param cwd string
---@return string[]|nil dirs
local function list_subdirs(cwd)
  local dirs = {}
  local has_fd = (vim.fn.executable "fd" == 1) or (vim.fn.executable "fdfind" == 1)
  if not has_fd then
    vim.notify("You need to install fd cli", "warn", { title = "Program missing" })
    return nil
  end
  if has_fd then
    local bin = vim.fn.executable "fd" == 1 and "fd" or "fdfind"
    -- requires nvim 0.10+
    local res = vim.system({ bin, "-t", "d" }, { cwd = cwd, text = true }):wait()
    if res and res.code == 0 and res.stdout then
      for line in res.stdout:gmatch "[^\r\n]+" do
        if line ~= "." and line ~= "" then table.insert(dirs, line) end
      end
    end
  end
  table.sort(dirs)
  -- Always put current directory first selection
  table.insert(dirs, 1, ".")
  return dirs
end

---Create directories if they don't exist
---@param path string
local function ensure_parent(path)
  local parent = vim.fs.dirname(path)
  if parent and parent ~= "" then vim.fn.mkdir(parent, "p") end
end

---@param p string
local function is_path_absolute(p) return type(p) == "string" and p:sub(1, 1) == "/" end

---@param path string
---@return boolean is_created true if file is created else false
local function create_empty_file(path)
  local fd = uv.fs_open(path, "w", 420)
  if not fd then return false end
  uv.fs_close(fd)
  return true
end

local function pick_dir_and_create_file()
  local cwd = vim.uv.cwd()
  if not cwd then return end
  local dirs = list_subdirs(cwd)
  if not dirs or next(dirs) == nil then
    vim.notify("Something went wrong!", "warn")
    return
  end
  vim.ui.select(dirs, {
    prompt = "Select directory",
    format_item = function(item)
      if item == "." then return "[.] current directory" end
      return item
    end,
  }, function(choice)
    if not choice then return end
    local base_dir = choice == "." and cwd or vim.fs.joinpath(cwd, choice)
    vim.ui.input({ prompt = "New file path (relative to " .. base_dir .. "):" }, function(input)
      if not input then return end
      input = input:gsub("^%./", ""):gsub("/+$", "")
      if input == "" then
        vim.notify("No file name provided.", "warn", { title = "CreateFileInDir" })
        return
      end
      local full_path = ""
      if is_path_absolute(input) then
        full_path = input
      else
        full_path = vim.fs.joinpath(base_dir, input)
      end
      local st = uv.fs_stat(full_path)
      if st and st.type == "directory" then
        vim.notify("Path is a directory: " .. full_path, "warn", { title = "CreateFileInDir" })
        return
      end
      ensure_parent(full_path)
      if not st then
        local ok = create_empty_file(full_path)
        if not ok then
          vim.notify("Failed to create file: " .. full_path, "warn", { title = "CreateFileInDir" })
          return
        end
        vim.cmd.edit(vim.fn.fnameescape(full_path))
        vim.notify("Opened: " .. full_path, "info", { title = "CreateFileInDir" })
      end
    end)
  end)
end

vim.api.nvim_create_user_command(
  "CreateFileInDir",
  pick_dir_and_create_file,
  { desc = "Launch picker to select a directory and then prompt to create a file." }
)

vim.keymap.set("n", "<leader>,", "<cmd>CreateFileInDir<cr>")
