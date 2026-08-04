local o = vim.opt
local g = vim.g

g.mapleader = " "
g.maplocalleader = "\\"

o.number = true
o.relativenumber = true
o.cursorline = true
o.cursorlineopt = "number"
o.scrolloff = 8
o.sidescrolloff = 8

o.expandtab = true
o.tabstop = 2
o.shiftwidth = 2
o.softtabstop = -1
o.smarttab = true
o.shiftround = true

o.autoindent = true
o.smartindent = true

o.wildignorecase = true
o.wildmenu = true
o.wildmode = "longest:full,full"

o.ignorecase = true
o.smartcase = true
o.tagcase = "followscs"

o.jumpoptions = "clean,view"
o.iskeyword:append "-"
o.exrc = true

o.splitright = true
o.splitbelow = true
o.splitkeep = "topline"
o.laststatus = 3
o.showmode = false
o.ruler = false
o.winborder = "rounded"

o.numberwidth = 3
o.signcolumn = "yes:1"
-- o.statuscolumn = "%!v:lua.require'config.statuscolumn'.render()"
o.smoothscroll = true
o.termguicolors = true
o.cmdheight = 0

o.wrap = true
o.breakindent = true
o.linebreak = true
o.showbreak = "󰄾 "

o.confirm = true
o.backup = false
o.writebackup = true
o.swapfile = true
o.undofile = true
o.undolevels = 10000
o.updatetime = 300
o.timeoutlen = 500
o.virtualedit = "block"
o.matchpairs:append "<:>"

o.fillchars = {
  eob = " ",
  foldopen = "",
  foldclose = "", -- fold close icon
  foldsep = " ",
  foldinner = vim.fn.has "nvim-0.12" == 1 and " " or nil,
}
o.foldcolumn = "1"
o.foldenable = true
o.foldlevel = 99
o.foldlevelstart = 99
o.foldtext = ""

o.fillchars:append({ eob = " " })
o.path:append({ "**" })
o.shortmess:append "c"

o.diffopt:append({ "algorithm:histogram", "linematch:60" })
g.markdown_recommended_style = 0
g.tsc_makeprg = "npx tsc"

if vim.fn.executable "rg" == 1 then
  o.grepprg = "rg --vimgrep --smart-case -uu --sort=path"
end

if vim.fn.executable "fish" == 1 then
  o.shell = "fish"
end

vim.schedule(function()
  local is_ssh = vim.env.SSH_CONNECTION ~= nil or vim.env.SSH_CLIENT ~= nil or vim.env.SSH_TTY ~= nil
  if not is_ssh then
    o.clipboard:append({ "unnamedplus" })
  end

  vim.filetype.add({
    filename = {
      dwm_sxhkdrc = "sxhkdrc",
    },
    pattern = {
      [".env*"] = "conf",
      ["tsconfig*.json"] = "jsonc",
      [".*/kitty/.+%.conf"] = "kitty",
    },
  })

  vim.diagnostic.config({
    virtual_text = true,
    virtual_lines = false,
    float = false,
    signs = {
      text = {
        [vim.diagnostic.severity.ERROR] = "",
        [vim.diagnostic.severity.HINT] = "",
        [vim.diagnostic.severity.INFO] = "",
        [vim.diagnostic.severity.WARN] = "",
      },
    },
    underline = false,
    severity_sort = true,
  })
  if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then
    vim.opt.findfunc = "v:lua.Fd"
  end
  if vim.fn.has "nvim-0.12" == 1 then
    vim.o.pumborder = "rounded"

    local ui2_ok, ui2 = pcall(require, "vim._core.ui2")
    if ui2_ok then
      ui2.enable({ msg = { targets = "cmd" } })
    end
  end
end)

function _G.Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then
    file_pattern = file_pattern:gsub(".", ".*%0") .. ".*"
  end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
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
