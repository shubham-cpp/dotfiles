-- local au_files = vim.api.nvim_create_augroup("SP_Files", { clear = true })
-- Why? Because by default vim will copy the * search to clipboard
vim.cmd([[function! GetVisualSelection() abort
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
]])

-- vim.api.nvim_create_autocmd("FileType", {
-- 	group = au_files,
-- 	pattern = { "markdown" },
-- 	callback = function()
-- 		vim.opt_local.colorcolumn = "100"
-- 		vim.opt_local.textwidth = 100
-- 		vim.opt_local.spell = true
-- 		vim.opt_local.linebreak = true
-- 		vim.opt_local.spelllang = "en"
-- 		vim.opt_local.concealcursor = "nv"
-- 		vim.opt_local.formatoptions = "tcqjn"
-- 		vim.opt_local.formatlistpat = "^\\s*\\([~]\\+\\|[-]\\+\\)\\s*"
-- 		-- ^\s*\d\+\.\s\+\|^\s*[-*+]\s\+\|^\[^\ze[^\]]\+\]:
-- 	end,
-- 	desc = "Setup for markdown files",
-- })

vim.filetype.add({
	extension = {
		pcss = "css",
	},
})
