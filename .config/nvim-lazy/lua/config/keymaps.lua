-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("", "0", "^", { silent = false })
vim.keymap.set("n", ",w", "<cmd>w!<cr>", { desc = "Save file" })
vim.keymap.set("n", ",W", "<cmd>noautocmd w!<cr>", { desc = "Save file(noautocmd)" })

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

vim.keymap.set("c", "<C-a>", "<Home>", { silent = false })
vim.keymap.set("c", "<C-e>", "<End>", { silent = false })

-- Pasting in visual mode doesn't copy
vim.keymap.set("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })
vim.keymap.set("n", "dl", '"_dl')
vim.keymap.set("v", "D", '"_D')
vim.keymap.set({ "n", "v" }, "c", '"_c')
vim.keymap.set("n", "C", '"_C')

for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto Tab " .. i })
end
if _G.Snacks then
  Snacks.toggle.zoom():map("<C-w>m")
end
