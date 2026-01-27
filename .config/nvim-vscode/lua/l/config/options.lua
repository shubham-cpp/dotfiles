local o = vim.opt
local g = vim.g

local rtps = vim.api.nvim_list_runtime_paths()
local all_comps = {}
for _, p in ipairs(rtps) do
  for _, f in ipairs(vim.fn.globpath(p, "compiler/*.vim", 0, 1)) do
    table.insert(all_comps, vim.fn.fnamemodify(f, ":t:r"))
  end
end

o.tabstop = 2
o.shiftwidth = 2
o.softtabstop = -1
o.expandtab = true
o.number = true
o.relativenumber = true
o.autoindent = true
o.smartindent = true
o.ignorecase = true
o.smartcase = true
o.mouse = "a"
o.iskeyword:append "-"
o.path:append "**"
o.backup = true

o.undofile = true
o.undolevels = 10000

o.wildignorecase = true
o.splitbelow = true
o.splitright = true
o.undodir = { vim.fn.stdpath "state" .. "/undodir//" }
o.directory = { vim.fn.stdpath "state" .. "/swap//" }
o.backupdir = { vim.fn.stdpath "state" .. "/backup//" }
o.sessionoptions:append({
  "globals",
})
o.sessionoptions:remove "folds"
o.jumpoptions = "stack"
vim.opt.shortmess:append "IWc"

vim.schedule(function()
  o.clipboard:append(vim.env.SSH_TTY and "" or "unnamedplus") -- Sync with system clipboard
end)

if vim.fn.executable "rg" == 1 then
  o.grepprg = "rg --vimgrep --no-heading --smart-case --sort=path"
  ---  set grepformat+=%f:%l:%c:%m
end

if not vim.g.vscode then
  o.scrolloff = 12
  o.wildmode = "longest:full,full"
  o.signcolumn = "yes"
else
  o.cmdheight = 8
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
if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then
  vim.opt.findfunc = "v:lua.Fd"
end

vim.api.nvim_create_user_command("Redir", function(opts)
  local cmd = opts.args
  local output

  if cmd:match "^!" then
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
  vim.cmd "$tabnew"
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

vim.api.nvim_create_user_command("RunMake", function(opts)
  vim.cmd "update"
  local compiler = opts.fargs[1]
  vim.cmd("compiler " .. compiler)
  -- If there are more arguments, pass them to Make
  if #opts.fargs > 1 then
    -- Join remaining args and append to Make
    local make_args = table.concat(vim.list_slice(opts.fargs, 2), " ")
    vim.cmd("Make " .. make_args)
  else
    vim.cmd "Make"
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
