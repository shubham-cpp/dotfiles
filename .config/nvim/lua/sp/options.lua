local o = vim.opt
local cmd = vim.cmd
local g = vim.g

g.mapleader = ' '
g.maplocalleader = '\\'
o.number = true
o.relativenumber = true
o.clipboard:append({ 'unnamedplus' })
o.mouse = 'a'
o.path:append({ '**' })
o.splitbelow = true
o.splitright = true
o.spell = false
o.backupdir = vim.fn.stdpath('cache') .. '/backups//'
o.backup = true
o.backupcopy = 'yes'
o.writebackup = true
o.swapfile = false
o.history = 500
o.cmdheight = 2
o.updatetime = 300
o.shortmess:append('c')
o.signcolumn = 'yes'
o.iskeyword:append('-')
o.wildignorecase = true
o.wildmode = 'longest,list,full'
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
o.tagcase = 'smart'
o.lazyredraw = true
o.showmatch = true
o.whichwrap:append('<,>,[,]')
o.timeoutlen = 500
o.errorbells = false
o.visualbell = false
o.foldcolumn = '1'
o.colorcolumn = '80'
o.scrolloff = 8
o.expandtab = true
o.shiftwidth = 4
o.tabstop = 4
o.cursorline = true
o.wrap = true
o.breakindent = true
o.linebreak = true
o.showbreak = '>> '
o.numberwidth = 6
o.list = false
o.smartindent = true
o.showtabline = 2
o.showmode = false
o.termguicolors = true
o.tags:append({ './.git/tags' })
o.inccommand = 'nosplit'
o.undofile = true
o.laststatus = 3
o.winbar = '%f %m'
g.session_directory = string.format('%s/session', vim.fn.stdpath('data'))
g.session_autoload = 'no'
g.session_autosave = 'no'
g.session_command_aliases = 1
o.encoding = 'utf-8'
o.fileencoding = 'utf-8'
if vim.fn.executable('rg') == 1 then
  o.grepprg = 'rg --vimgrep --smart-case --hidden --follow'
end

cmd([[ command! Q :q! ]])
cmd([[ cabbrev vf vert sf ]])

-- Navigate between matching html tags
-- Disable options {{{

g.python3_host_skip_check = 1
g.python3_host_prog = '/usr/bin/python'

local disabled_built_ins = {
  'netrw',
  'netrwPlugin',
  'netrwSettings',
  'netrwFileHandlers',
  'gzip',
  'zip',
  'zipPlugin',
  'tar',
  'tarPlugin',
  'getscript',
  'getscriptPlugin',
  'vimball',
  'vimballPlugin',
  '2html_plugin',
  'logipat',
  'rrhelper',
  'spellfile_plugin',
  'matchit',
}

for _, plugin in pairs(disabled_built_ins) do
  vim.g['loaded_' .. plugin] = 1
end

-- }}}
-- File Specific options {{{
g.rust_conceal = 1
g.rust_conceal_mod_path = 1
g.rust_fold = 2
g.rustfmt_autosave = 1
g.rubycomplete_classes_in_global = 1
g.rubycomplete_rails = 1
g.python_space_error_highlight = 1
g.msql_sql_query = 1
g.lite_sql_query = 1
g.lisp_rainbow = 1
g.lifelines_deprecated = 1
g.vim_json_warnings = 1
g.java_highlight_debug = 1
g.java_highlight_functions = 'style'
g.hs_highlight_debug = 1
g.hs_highlight_types = 1
g.hs_highlight_more_types = 1
g.hs_highlight_boolean = 1
g.hs_highlight_delimiters = 1
g.erlang_highlight_special_atoms = 1
g.doxygen_enhanced_color = 1
g.desktop_enable_nonstd = 1
g.desktop_enable_kde = 1
g.clojure_fold = 1
g.c_curly_error = 1
g.c_space_errors = 1
g.c_comment_strings = 1
g.c_gnu = 1
g.sh_fold_enabled = 4
g.vimsyn_embed = 'lP'
-- }}}
