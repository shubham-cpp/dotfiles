local o = vim.opt
local g = vim.g
local cmd = vim.cmd

o.number = true
o.relativenumber = true
o.clipboard:append({ 'unnamedplus' })
o.mouse = 'a'
o.path:append({ '**' })
o.splitbelow = true
o.splitright = true
o.fileignorecase = true
o.showmode = false -- Dont show mode since we have a statusline
o.inccommand = 'nosplit'
o.formatoptions = 'jcroqlnt'
o.backupdir = vim.fn.stdpath 'cache' .. '/backups//'
-- o.backup = true
-- o.backupcopy = 'yes'
-- o.writebackup = true
o.swapfile = true
o.undofile = true
o.undolevels = 10000
o.directory = vim.fn.stdpath 'cache' .. '/swaps//'
o.undodir = vim.fn.stdpath 'cache' .. '/undo//'
-- o.history = 500
-- o.cmdheight = 1
-- o.updatetime = 300
-- o.shortmess:append 'c'
-- o.signcolumn = 'yes'
-- o.iskeyword:append '-'
o.wildignorecase = true
-- o.wildmode = 'list:lastused'
o.wildignore:prepend({
  '*.out,*.o,*.pyc,*~,*.class,*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store,*/venv/*,*/__pycache__/*,*.jpg,*.png,*.svg,*.jpeg,*.jpg',
  '*.mp4',
  '**/node_modules/*',
  '**/build/*',
  '**/coverage/*',
  '*.mkv',
})
o.ignorecase = true
o.smartcase = true
-- o.tagcase = 'smart'
-- o.lazyredraw = true
-- o.showmatch = true
o.whichwrap:append '<,>,[,]'
-- o.timeoutlen = 500
-- o.errorbells = false
-- o.visualbell = false
o.scrolloff = 8
o.expandtab = true
o.shiftwidth = 2
o.tabstop = 2
-- o.cursorline = true
-- o.wrap = true
-- o.breakindent = true
-- o.linebreak = true
-- o.showbreak = '>> '
-- o.numberwidth = 6
-- o.list = false
-- o.smartindent = true
-- o.showtabline = 2
-- o.showmode = false
if vim.fn.has 'termguicolors' == 1 then
  o.termguicolors = true
end
-- o.tags:append({ './.git/tags' })
-- o.laststatus = 3
if vim.fn.executable 'rg' == 1 then
  o.grepformat = '%f:%l:%c:%m'
  o.grepprg = 'rg --vimgrep --smart-case --hidden --follow'
end

-- cmd.colorscheme 'everforest'

cmd [[ command! Q :q! ]]
cmd [[ cabbrev vf vert sf ]]