local opt = vim.opt
local g = vim.g

-- Indentation & Tabs
opt.expandtab = true
opt.tabstop = 2
opt.shiftwidth = 2
opt.smartindent = true
opt.shiftround = true
opt.copyindent = true
opt.preserveindent = true

-- Search & Completion
opt.ignorecase = true
opt.smartcase = true
opt.completeopt = "menu,menuone,noselect"
opt.infercase = true
opt.wildignorecase = true
opt.pumheight = 10
opt.shortmess:append({ s = true, I = true, c = true, C = true })

-- Clipboard & Undo
opt.clipboard = "unnamedplus"
opt.undofile = true
opt.undolevels = 15000

-- UI & Display
opt.number = true
opt.relativenumber = true
opt.cursorline = true
opt.signcolumn = "yes"
opt.laststatus = 3
opt.showmode = false
opt.ruler = false
opt.cmdheight = 0
opt.showtabline = 2
opt.title = true
opt.termguicolors = true
opt.wrap = true
opt.linebreak = true
opt.breakindent = true
opt.showbreak = "  "
opt.fillchars = {
  eob = " ",
  foldopen = "",
  foldclose = "",
  foldinner = " ",
  fold = " ",
  foldsep = " ",
}
opt.foldcolumn = "1"
opt.foldlevel = 99
opt.foldmethod = "indent"
opt.foldtext = ""
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.confirm = true

-- Splits & Windows
opt.splitbelow = true
opt.splitright = true
opt.splitkeep = "screen"
opt.virtualedit = "block"
opt.winminwidth = 5

-- Editing Behavior
opt.mouse = "a"
opt.timeoutlen = 300
opt.updatetime = 200
opt.jumpoptions = "view"
opt.backspace:append({ "nostop" })
opt.diffopt:append({ "algorithm:histogram", "linematch:60" })
opt.whichwrap:append("<>[]hl")
opt.writebackup = false
opt.iskeyword:append("-")

-- File & Session Management
opt.autowrite = true
opt.sessionoptions = "buffers,curdir,tabpages,winsize,help,globals,skiprtp"
opt.exrc = true
opt.wildignore:append({
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

-- Grep & Find
if vim.fn.executable("rg") == 1 then
  opt.grepprg = "rg --vimgrep --smart-case --no-heading --sort=path"
end

function _G.Fd(file_pattern, _)
  if file_pattern:sub(1, 1) == "*" then
    file_pattern = file_pattern:gsub(".", ".*%0") .. ".*"
  end
  local cmd = 'fd --color=never --full-path --type file "' .. file_pattern .. '"'
  return vim.fn.systemlist(cmd)
end

if vim.fn.executable("fd") == 1 then
  opt.findfunc = "v:lua.Fd"
end

-- Globals
g.markdown_recommended_style = 0
g.loaded_node_provider = 0
g.loaded_python3_provider = 0
g.loaded_perl_provider = 0
g.loaded_ruby_provider = 0

-- Custom filetype detection
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

vim.cmd("packadd cfilter")
vim.cmd("packadd nvim.undotree")
vim.cmd("packadd nvim.tohtml")
