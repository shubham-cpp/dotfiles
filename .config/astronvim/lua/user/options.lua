local o = vim.opt

vim.cmd [[ command! Q :q! ]]
vim.cmd [[ cabbrev vf vert sf ]]

local options = {
  opt = {
    path = o.path + '**',
    tabstop = 2,
    softtabstop = 2,
    shiftwidth = 2,
    expandtab = true,
    backup = true,
    backupdir = vim.fn.stdpath 'cache' .. '/backups//',
    backupcopy = 'yes',
    writebackup = true,
    swapfile = true,
    shortmess = o.shortmess + 'c',
    iskeyword = o.iskeyword + 'c',
    wildignorecase = true,
    wildmode = 'longest,list,full',
    wildignore = {
      '*.out,*.o,*.pyc,*~,*.class,*/.git/*,*/.hg/*,*.jpg,*.png,*.svg,*.jpeg,*.jpg',
      '*.mp4,*.mkv,*/.svn/*,*/.DS_Store,*/venv/*,*/__pycache__/*,',
      '**/node_modules/*,**/build/*,**/coverage/*',
    },
    whichwrap = o.whichwrap + '<,>,[,]',
    colorcolumn = '120',
    wrap = true,
    breakindent = true,
    linebreak = true,
    showbreak = '>> ',
    grepprg = vim.fn.executable 'rg' == 1 and 'rg --vimgrep --smart-case --hidden --follow' or o.grepprg,
  },
}
return options
