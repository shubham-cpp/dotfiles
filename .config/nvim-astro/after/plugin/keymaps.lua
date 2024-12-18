vim.keymap.set("", "0", "^", { noremap = false, silent = true })
vim.keymap.set("n", ",w", "<cmd>w!<cr>", { desc = "Save File" })
vim.keymap.set("n", ",W", "<cmd>noautocmd w!<cr>", { desc = "Save File(without autocmds)" })
vim.keymap.set("n", "<Esc>", ":nohl<cr>")

vim.keymap.set("n", "<S-Left>", ":tabp<cr>")
vim.keymap.set("n", "<S-Right>", ":tabn<cr>")

-- Pasting in visual mode doesn't copy
vim.keymap.set("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })
vim.keymap.set("n", "dl", '"_dl')
vim.keymap.set({ "n", "x" }, "c", '"_c')
vim.keymap.set("n", "C", '"_C')

vim.keymap.set("c", "<C-a>", "<Home>", { silent = false })
vim.keymap.set("c", "<C-e>", "<End>", { silent = false })

vim.cmd [[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]]
vim.keymap.set("x", ",s", '"sy:let @/=@s<CR>cgn', { desc = "Replace" })

vim.keymap.set("o", "ie", ':exec "normal! ggVG"<cr>')
vim.keymap.set("o", "iv", ':exec "normal! HVL"<cr>')
vim.keymap.set("n", ",e", ':e <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = "Edit in same dir" })
vim.keymap.set("n", ",t", ':tabe <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = "Edit in same dir(Tab)" })
vim.keymap.set(
  "n",
  ",v",
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir(Split)" }
)

vim.keymap.set("n", "<leader>R", function()
  vim.cmd "source %"
  local file = vim.fn.substitute(vim.fn.expand "%:r", "lua/", "", "")
  local ok, mod = pcall(require, file)
  if ok and type(mod) ~= "boolean" and next(mod or {}) ~= nil and mod.config then mod.config() end
end, { silent = false, desc = "Reload module" })

for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto Tab " .. i })
end
-- Output of neovim/vim command into buffer
-- enew|pu=execute('map')
