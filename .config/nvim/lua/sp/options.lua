local o = vim.opt
local g = vim.g

g.mapleader = ' '
o.clipboard:append 'unnamedplus'
o.iskeyword:append '-'
o.number = true
o.relativenumber = true
o.expandtab = true
o.tabstop = 2
o.shiftround = true
o.shiftwidth = 2
o.smarttab = true
o.autoindent = true
o.smartindent = true
o.incsearch = true
o.smartcase = true
o.hlsearch = true
o.ignorecase = true
o.splitright = true
o.splitbelow = true
o.path:append '**'
o.cursorline = true
o.inccommand = 'nosplit'
o.scrolloff = 16
o.sidescrolloff = 8
o.shortmess:append({ W = true, I = true, c = true })
o.showmode = false -- Dont show mode since we have a statusline
o.formatoptions = 'jcrqlnt'
if vim.fn.executable 'rg' == 1 then
  o.grepformat = '%f:%l:%c:%m'
  o.grepprg = 'rg --vimgrep --no-heading --smart-case'
  -- o.grepprg = 'rg --vimgrep --no-heading --smart-case --type-add vue:*.vue --type-add svelte:*.svelte --type-add prisma:*.prisma'
end
o.timeoutlen = 300
o.swapfile = true
o.undofile = true
o.undolevels = 10000
o.undodir = vim.fn.stdpath 'state' .. '/undodir//'
o.directory = vim.fn.stdpath 'state' .. '/swap//'
o.backupdir = vim.fn.stdpath 'state' .. '/backup//'
o.updatetime = 200
o.wildmode = 'longest:full,full'
o.wildignorecase = true
o.wildignore:append({
  '**/node_modules/*',
  '**/.git/*',
  '**/dist/*',
  '**/build/*',
  '**/static/*',
  '**/.next/*',
  '*.o',
  '*.out',
  '*.obj',
  '*.exe',
  '*.dll',
  '*.jar',
  '*.pyc',
  '*.rbc',
  '*.class',
  '*.gif',
  '*.ico',
  '*.jpg',
  '*.jpeg',
  '*.png',
  '*.mov',
  '*.mht',
  '*.swp',
  '*.zip',
  '*.tar.gz',
  '*.tar.bz2',
})
-- o.winminwidth = 5
o.foldcolumn = '1' -- '0' is not bad
o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
o.foldlevelstart = 99
o.foldenable = true
o.numberwidth = 3
-- credits - https://github.com/neovim/neovim/pull/17446
-- o.statuscolumn='%{foldlevel(v:lnum) > 0 ? (foldlevel(v:lnum) > foldlevel(v:lnum - 1) ? (foldclosed(v:lnum) == -1 ? " " : " ") : "") : " " }%=%l%s'
o.statuscolumn =
  '%{foldlevel(v:lnum) > 0 ? (foldlevel(v:lnum) > foldlevel(v:lnum - 1) ? (foldclosed(v:lnum) == -1 ? "" : "") : "|") : " " }%=%{v:relnum?v:relnum:v:lnum}%s '

if vim.fn.has 'nvim-0.9' == 1 then
  o.splitkeep = 'screen'
  o.shortmess:append({ C = true })
  o.backspace:append({ 'nostop' })
  o.diffopt:append 'linematch:60'
end

vim.cmd 'filetype plugin indent on'
vim.t['bufs'] = vim.t.bufs and vim.t.bufs or vim.api.nvim_list_bufs() -- initialize buffers for the current tab

-- Fix markdown indentation settings
g.markdown_recommended_style = 0
