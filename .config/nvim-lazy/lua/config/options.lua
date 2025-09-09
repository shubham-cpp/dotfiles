-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.opt.wrap = true
vim.opt.showbreak = "󰄾 "
vim.opt.iskeyword:append("-")
vim.opt.scrolloff = 8
vim.opt.sessionoptions = "buffers,curdir,tabpages,winsize,help,globals,skiprtp"
vim.opt.exrc = true

vim.opt.wildignore:append({
  "*.o",
  "*.obj",
  "*.out",
  "*.class",

  "*.jpg",
  "*.jpeg",
  "*.png",
  "*.gif",

  "*.zip",
  "*.tar.gz",
  "*.tar.xz",
  "*.tar.bz2",
  "*.rar",

  "*.pyc",
  "*.pyo",

  "*.DS_Store",
  "*.swp",

  "node_modules",
  "vendor",
  ".git",
})

if vim.fn.executable("rg") == 1 then
  vim.opt.grepprg = "rg --vimgrep --smart-case --no-heading --sort=path" -- Also check RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"
end

function _G.Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then
    file_pattern = file_pattern:gsub(".", ".*%0") .. ".*"
  end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
end
if vim.fn.has("nvim-0.11") == 1 and vim.fn.executable("fd") == 1 then
  vim.opt.findfunc = "v:lua.Fd"
end

vim.api.nvim_create_user_command("Redir", function(opts)
  local cmd = opts.args
  local output

  if cmd:match("^!") then
    -- Run shell command (strip !)
    local shell_cmd = cmd:sub(2)
    output = vim.split(vim.fn.system(shell_cmd), "\n", { trimempty = true })
  else
    -- Redirect built-in/ex command output
    local ok, result = pcall(vim.api.nvim_exec2, cmd, { output = true })
    if not result.output then
      return
    end
    output = ok and vim.split(result.output, "\n", { trimempty = false }) or { result }
  end

  -- Open new tab with scratch buffer
  vim.cmd("$tabnew")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
  vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })
  vim.api.nvim_set_option_value("swapfile", false, { buf = buf })
  vim.api.nvim_set_option_value("buflisted", false, { buf = buf })

  -- Show the command and separator
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { string.format("Command [[ %s ]] ----- Output ---->", cmd) })
  -- Populate lines
  vim.api.nvim_buf_set_lines(buf, 1, -1, false, output)
end, {
  nargs = 1,
  desc = "Redirect output of a command to scratch tab",
  complete = function(query)
    return vim.fn.getcompletion(query, "command")
  end,
})

vim.api.nvim_create_user_command("PrintConfig", function(opts)
  local plugins = vim.tbl_keys(require("lazy.core.config").plugins)
  local args = opts.args
  local function callback(plugin_name)
    local cmd = "Redir lua =require('lazy.core.config').plugins['" .. plugin_name .. "']"
    vim.notify(cmd, vim.log.levels.INFO, { title = "Command" })
    vim.fn.execute(cmd)
  end
  if args ~= "" then
    callback(args)
    return
  end

  vim.ui.select(plugins, { prompt = "Select Config to print" }, function(item)
    if not item then
      return
    end
    callback(item)
  end)
end, {
  desc = "Print final lazy config",
  nargs = "?",
  complete = function(prefix)
    local plugins = vim.tbl_keys(require("lazy.core.config").plugins)
    return vim
      .iter(plugins)
      :filter(function(t)
        if string.len(prefix:gsub("%s+", "")) > 0 then
          return t:match(prefix)
        end
        return true
      end)
      :totable()
  end,
})

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
  -- If there are more arguments, pass them to Make
  if #opts.fargs > 1 then
    -- Join remaining args and append to Make
    local make_args = table.concat(vim.list_slice(opts.fargs, 2), " ")
    vim.cmd("Make " .. make_args)
  else
    vim.cmd("Make")
  end
end, {
  nargs = "+",
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
