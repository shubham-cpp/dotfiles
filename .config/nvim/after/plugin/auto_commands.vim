augroup Highlight_Yank " {{{
    autocmd!
    au TextYankPost * silent! lua vim.highlight.on_yank()
    au FileType vim,lua set foldmethod=marker
    au FileType vim,lua nnoremap <buffer> <silent> <F5> :so %<cr>
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
    au TermOpen term://* startinsert
augroup END " }}}

augroup Dynamic_Smartcase " {{{
    autocmd!
    au CmdLineEnter : set nosmartcase
    au CmdLineLeave : set smartcase
augroup END "}}}

" For Filehandling {{{
augroup File_Reloads
    autocmd!
    au BufWritePost Xresources,Xdefaults !xrdb %
    au BufEnter *.fish set filetype=fish
    au BufEnter *profile set filetype=sh
    au BufEnter vifmrc set filetype=vim
    au BufWritePost sxhkdrc call system('pkill -USR1 -x sxhkd && notify-send -t 1000 BSPWM SXHKD\ Restarted')
    au BufEnter dwm_sxhkdrc,awesome_keys set ft=sxhkd
    " Automatically deletes all trailing whitespace and newlines at end of file on save.
    au BufWritePre * %s/\s\+$//e
    au BufWritepre * %s/\n\+\%$//e
    " update buffer if changed from outside
    au FocusGained,BufEnter * checktime
    au FileType css,javascript,html,json setlocal sw=2 ts=2
    "Meaningful backup name, ex: filename@2015-04-05.14:59
    au BufWritePre * let &bex = '@' . strftime("%F.%H:%M")
augroup end " }}}

" Auto formattings {{{
augroup LspFormattings
    autocmd!
    " au BufWritePre *.js,*.jsx,*.tsx,*.ts lua vim.lsp.buf.formatting_sync({tabSize=2,trimFinalNewlines=true,trimTrailingWhitespace=true})
    au BufWritePre *.html,*.css,*.scss lua vim.lsp.buf.formatting_sync({tabSize=2,trimFinalNewlines=true,trimTrailingWhitespace=true})
    au BufWritePre *.lua,*.go,*.py lua vim.lsp.buf.formatting_sync({tabSize=2,trimFinalNewlines=true,trimTrailingWhitespace=true})
    au BufWritePre *.json,*.md lua vim.lsp.buf.formatting_sync({tabSize=2,trimFinalNewlines=true,trimTrailingWhitespace=true})
augroup END
" }}}

augroup Comments " {{{
    autocmd!
    au FileType apache,sxhkdrc,toml,fish setlocal commentstring=#\ %s
    au FileType lisp setlocal commentstring=;;\ %s
    au FileType htmldjango setlocal commentstring=<!--%s-->
    au FileType xdefaults setlocal commentstring=!\ %s
    au BufEnter *.vifm setlocal commentstring=\"\ %s
    au BufEnter template,mbsyncrc setlocal commentstring=##\ %s
    au BufEnter *.kbd setlocal ft=lisp | setlocal commentstring=;;\ %s
    au FileType * set formatoptions-=c formatoptions-=r formatoptions-=o
    au FileType c,cpp setlocal comments-=:// comments+=f://
augroup end " }}}

augroup Plugin_Confs "{{{
    autocmd!
    " au FileType vim,json let g:argwrap_line_prefix='\'
    " au FileType lua let g:argwrap_line_prefix=' '
    " au FileType java lua require("lsp/jdt_setup").setup()

    " au VimEnter * silent exec "!kill -s SIGWINCH $PPID"
    autocmd BufWritePost packer-plugins.lua source <afile> | PackerCompile
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
augroup END "}}}

" Some commands from vimfandom {{{
" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
            \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
            \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
            \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
            \gVzv:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
            \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
            \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
            \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
            \gVzv:call setreg('"', old_reg, old_regtype)<CR>
"}}}
