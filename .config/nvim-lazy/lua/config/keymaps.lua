-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.del("n", "<leader>l")

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
-- floating terminal
local lazyterm = function()
  LazyVim.terminal(nil, { cwd = LazyVim.root() })
end
vim.keymap.del({ "n", "t" }, "<c-/>")
vim.keymap.del("t", "<c-l>")
vim.keymap.del({ "n", "t" }, "<c-_>")
vim.keymap.set("n", "<c-\\>", lazyterm, { desc = "Open terminal" })
-- vim.keymap.set("n", "<c-/>", function()
--   LazyVim.terminal()
-- end, { desc = "Terminal (cwd)" })
vim.keymap.set("n", "<c-\\>", lazyterm, { desc = "Open terminal" })
vim.keymap.set("t", "<c-]>", "<C-\\><C-n>", { desc = "Terminal Escape mode" })
vim.keymap.set("t", "<c-\\>", "<cmd>close<cr>", { desc = "Hide Terminal" })
vim.keymap.set("t", "<c-/>", "<cmd>close<cr>", { desc = "Hide Terminal" })

for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto Tab " .. i })
end

vim.keymap.set("n", "<leader>tf", function()
  LazyVim.terminal({ "yazi" }, { cwd = LazyVim.root.cwd(), esc_esc = false, ctrl_hjkl = false })
end, { desc = "Yazi (Root cwd)" })
vim.keymap.set("n", "<leader>tF", function()
  LazyVim.terminal({ "yazi" }, { esc_esc = false, ctrl_hjkl = false })
end, { desc = "Yazi" })
