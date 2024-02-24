-- Why? Because by default vim will copy the * search to clipboard
vim.cmd[[function! GetVisualSelection() abort
  try
    let a_save = @a
    silent! normal! gv"ay
    return @a
  finally
    let @a = a_save
  endtry
endfunction

vnoremap <silent> * <c-\><c-n>:let @/ = escape(GetVisualSelection(), '/\^$*.[~')<CR>/<CR>
vnoremap <silent> # <c-\><c-n>:let @/ = escape(GetVisualSelection(), '/\^$*.[~')<CR>?<CR>
]]
