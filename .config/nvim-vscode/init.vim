function! Cond(Cond, ...)
  let opts = get(a:000, 0, {})
  return a:Cond ? opts : extend(opts, { 'on': [], 'for': [] })
endfunction

let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
let g:qs_lazy_highlight = 1

call plug#begin(stdpath('data') . '/plugged')
" Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-abolish'
Plug 'unblevable/quick-scope'
" Plug 'svermeulen/vim-subversive'
Plug 'machakann/vim-swap'
Plug 'folke/flash.nvim'
Plug 'monaqa/dial.nvim'
Plug 'echasnovski/mini.nvim'
if !exists('g:vscode')
  " Plug 'masar3141/mono-jade'
  " Plug 'AlexvZyl/nordic.nvim', { 'branch': 'main' }
  " Plug 'mellow-theme/mellow.nvim', { 'branch': 'main' }
  Plug 'blazkowolf/gruber-darker.nvim', { 'branch': 'main' }
  Plug 'windwp/nvim-autopairs'
endif

call plug#end()
set nocompatible
set tabstop=2
set shiftwidth=2
set softtabstop=-1
set expandtab
set number
set relativenumber
set autoindent
set smartindent
set mouse=a
set iskeyword+=-
if !exists('g:vscode')
  " filetype plugin indent on
  " syntax on
  set path+=**
  set backup
  set scrolloff=12
  if has('termguicolors')
    set termguicolors
  endif
  set splitbelow splitright
  set background=dark
  set swapfile
  set undofile undolevels=10000
  set wildmode=longest:full,full
  set wildignorecase

  nnoremap <C-j> <C-w><C-j>
  nnoremap <C-k> <C-w><C-k>
  nnoremap <C-h> <C-w><C-h>
  nnoremap <C-l> <C-w><C-l>
endif
set ignorecase
set smartcase
if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
  " set grepformat+=%f:%l:%c:%m
endif
let g:mapleader = " "

if exists('g:vscode')
  map ze <Cmd>call VSCodeNotify('scrollLineDown')<CR>
  map zy <Cmd>call VSCodeNotify('scrollLineUp')<CR>

  map [d <Cmd>call VSCodeNotify('editor.action.marker.prev')<CR>
  map ]d <Cmd>call VSCodeNotify('editor.action.marker.next')<CR>
  map [D <Cmd>call VSCodeNotify('editor.action.marker.prevInFiles')<CR>
  map ]D <Cmd>call VSCodeNotify('editor.action.marker.nextInFiles')<CR>
  map [e <Cmd>call VSCodeNotify('editor.action.marker.prev')<CR>
  map ]e <Cmd>call VSCodeNotify('editor.action.marker.next')<CR>
  map [E <Cmd>call VSCodeNotify('editor.action.marker.prevInFiles')<CR>
  map ]E <Cmd>call VSCodeNotify('editor.action.marker.nextInFiles')<CR>

  map [c <Cmd>call VSCodeNotify('workbench.action.editor.previousChange')<CR>
  map ]c <Cmd>call VSCodeNotify('workbench.action.editor.nextChange')<CR>

  map [f <Cmd>call VSCodeNotify('search.action.focusPrevSearchResult')<CR>
  map ]f <Cmd>call VSCodeNotify('search.action.focusNextSearchResult')<CR>
  map [q <Cmd>call VSCodeNotify('search.action.focusPrevSearchResult')<CR>
  map ]q <Cmd>call VSCodeNotify('search.action.focusNextSearchResult')<CR>

  map [b <Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>
  map ]b <Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>

  map gD <Cmd>call VSCodeNotify('editor.action.revealDefinitionAside')<CR>
  map gr <Cmd>call VSCodeNotify('editor.action.goToReferences')<CR>

  map [s <Cmd>call VSCodeNotify('editor.action.toggleStickyScroll')<CR>
  map =< <Cmd>call VSCodeNotify('editor.action.trimTrailingWhitespace')<CR>
  map gx <Cmd>call VSCodeNotify('editor.action.openLink')<CR>

  xmap gc  <Plug>VSCodeCommentary
  nmap gc  <Plug>VSCodeCommentary
  omap gc  <Plug>VSCodeCommentary
  nmap gcc <Plug>VSCodeCommentaryLine

  map <C-d> 15jzz
  map <C-u> 15kzz

  nnoremap <C-w>o <cmd>call VSCodeNotify('workbench.action.closeOtherEditors')<cr>
  nnoremap <C-w>C <cmd>call VSCodeNotify('workbench.action.closeEditorsToTheRight')<cr>

  nnoremap <leader>1 <Cmd>call VSCodeNotify('workbench.action.openEditorAtIndex1')<CR>
  nnoremap <leader>2 <Cmd>call VSCodeNotify('workbench.action.openEditorAtIndex2')<CR>
  nnoremap <leader>3 <cmd>call VSCodeNotify('workbench.action.openEditorAtIndex3')<cr>
  nnoremap <leader>4 <cmd>call VSCodeNotify('workbench.action.openEditorAtIndex4')<cr>
  nnoremap <leader>5 <cmd>call VSCodeNotify('workbench.action.openEditorAtIndex5')<cr>
  nnoremap <leader>6 <cmd>call VSCodeNotify('workbench.action.openEditorAtIndex6')<cr>

  nnoremap <leader>- <Cmd>call VSCodeNotify('eslint.executeAutofix')<CR>
  nnoremap <leader>= <Cmd>call VSCodeNotify('editor.action.formatSelection')<CR>
  vnoremap <leader>= <Cmd>call VSCodeNotifyRangePos('editor.action.formatSelection', line("v"), line("."), col("v"), col("."), 1)<CR>
  nnoremap g= <Cmd>call VSCodeNotify('eslint.executeAutofix')<CR>

  nnoremap <leader>f <Cmd>lua require('vscode-neovim').action('workbench.action.findInFiles', { args = { query = vim.fn.expand('<cword>') } })<CR>
  xnoremap <leader>f <Cmd>lua require('vscode-neovim').action('workbench.action.findInFiles', { args = { query = vim.fn.getline(vim.fn.getpos("'<")[2],vim.fn.getpos("'>")[2]) } })<CR>

  nmap gb <Cmd>call VSCodeNotify('editor.action.addSelectionToNextFindMatch')<cr>
  nnoremap <C-Up> <Cmd>call VSCodeNotify('editor.action.insertCursorAbove')<cr>
  nnoremap <C-Down> <Cmd>call VSCodeNotify('editor.action.insertCursorBelow')<cr>

  nnoremap <C-Right> <Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>
  nnoremap <C-Left> <Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>

  nnoremap <leader>o <Cmd>call VSCodeNotify('workbench.action.openRecent')<CR>
  nnoremap ,w <Cmd>call VSCodeNotify('workbench.action.files.save')<cr>
  nnoremap ,W <Cmd>call VSCodeNotify('workbench.action.files.saveWithoutFormatting')<cr>
  nnoremap gh <Cmd>call VSCodeNotify('editor.action.showHover')<cr>
  " VsCode Folding
  map za <Cmd>call VSCodeNotify('editor.toggleFold')<CR>
  map zC <Cmd>call VSCodeNotify('editor.foldAll')<CR>
  map zO <Cmd>call VSCodeNotify('editor.unfoldAll')<CR>
  map zp <Cmd>call VSCodeNotify('editor.gotoParentFold')<CR>

  nnoremap <C-b> <cmd>call VSCodeNotify('workbench.explorer.fileView.toggleVisibility')<cr>
  nnoremap <C-j> <Cmd>call VSCodeNotify('workbench.action.terminal.toggleTerminal')<cr>

  nnoremap <leader>gg <Cmd>call VSCodeNotify('workbench.view.scm')<cr>
  nnoremap <leader>ge <Cmd>call VSCodeNotify('workbench.view.extensions')<cr>
  nnoremap <leader>gs <Cmd>call VSCodeNotify('workbench.view.search.toggleVisibility')<cr>
  nnoremap <leader>gf <Cmd>call VSCodeNotify('workbench.view.explorer')<cr>
  nnoremap <leader>gk <Cmd>call VSCodeNotify('workbench.action.openGlobalKeybindings')<cr>
  nnoremap <C-w><C-c> <Cmd>call VSCodeNotify('workbench.action.closeActiveEditor')<CR>k
  nnoremap <leader>R <cmd>call VSCodeNotify('workbench.action.reloadWindow')<cr>
  " imap <C-k> <Cmd>call VSCodeNotify('editor.action.insertLineBefore')<CR>
  nnoremap <A-h> <Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>
  nnoremap <A-l> <Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>

  nnoremap <leader>a <cmd>call VSCodeNotify('vscode-harpoon.addEditor')<cr>
  nnoremap <C-e> <cmd>call VSCodeNotify('vscode-harpoon.editEditors')<cr>
  nnoremap <M-p> <Cmd>call VSCodeNotify('vscode-harpoon.editorQuickPick')<CR>
  nnoremap '1 <cmd>call VSCodeNotify('vscode-harpoon.gotoEditor1')<cr>
  nnoremap '2 <cmd>call VSCodeNotify('vscode-harpoon.gotoEditor2')<cr>
  nnoremap '3 <cmd>call VSCodeNotify('vscode-harpoon.gotoEditor3')<cr>
  nnoremap '4 <cmd>call VSCodeNotify('vscode-harpoon.gotoEditor4')<cr>

  function! MoveCursor(direction) abort
    if(reg_recording() == '' && reg_executing() == '')
      return 'g'.a:direction
    else
      return a:direction
    endif
  endfunction
  nmap <expr> j MoveCursor('j')
  nmap <expr> k MoveCursor('k')
else
  nnoremap ,w <cmd>w!<cr>
  nnoremap ,W <cmd>noautocmd w!<cr>
  nnoremap zp vaBo^<Esc>
endif

" for quick-scope plugin
highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline

nmap 0 ^
nnoremap <Esc> <cmd>nohl<cr>

vnoremap c "_c
nnoremap c "_c
nnoremap C "_C
nnoremap dl "_dl
nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn
vnoremap ,s "sy:let @/=@s<CR>cgn
vnoremap <expr> p 'pgv"'.v:register.'y'
onoremap <silent> ie :exec "normal! ggVG"<cr>
onoremap <silent> iv :exec "normal! HVL"<cr>
" from vim-swap plugin
omap i, <Plug>(swap-textobject-i)
xmap i, <Plug>(swap-textobject-i)
omap a, <Plug>(swap-textobject-a)
xmap a, <Plug>(swap-textobject-a)

augroup highlight_yank
  autocmd!
  autocmd TextYankPost * silent! lua vim.highlight.on_yank { higroup="IncSearch", timeout=100 }
augroup END

" set clipboard+=unnamedplus
lua << EOF
vim.schedule(function()
  vim.opt.clipboard:append(vim.env.SSH_TTY and '' or 'unnamedplus') -- Sync with system clipboard
end)
if vim.g.vscode == nil then
  local o = vim.opt
  o.undodir = { vim.fn.stdpath 'state' .. '/undodir//' }
  o.directory = { vim.fn.stdpath 'state' .. '/swap//' }
  o.backupdir = { vim.fn.stdpath 'state' .. '/backup//' }
  o.sessionoptions:append({
  'globals',
  })
  o.sessionoptions:remove 'folds'
end
EOF
