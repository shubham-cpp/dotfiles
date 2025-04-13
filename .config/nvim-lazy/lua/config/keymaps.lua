-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("", "0", "^", { silent = false })
vim.keymap.set("n", ",w", "<cmd>w!<cr>", { desc = "Save file" })
vim.keymap.set("n", ",W", "<cmd>noautocmd w!<cr>", { desc = "Save file(noautocmd)" })

vim.keymap.set(
  "n",
  "<localleader>e",
  ':e <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir" }
)
vim.keymap.set(
  "n",
  "<localleader>t",
  ':tabe <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir(Tab)" }
)
vim.keymap.set(
  "n",
  "<localleader>v",
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir(Split)" }
)

-- Pasting in visual mode doesn't copy
vim.keymap.set("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })
vim.keymap.set("n", "dl", '"_dl')
vim.keymap.set("v", "D", '"_D')
vim.keymap.set({ "n", "v" }, "c", '"_c')
vim.keymap.set("n", "C", '"_C')

for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto Tab " .. i })
end

vim.keymap.del("n", "<leader>l")
vim.keymap.del("n", "<leader>cd")
vim.keymap.del("n", "<leader>cf")
vim.keymap.set("n", "<leader>L", "<cmd>Lazy<cr>")
vim.keymap.set({ "n", "v" }, "<leader>lf", function()
  LazyVim.format({ force = true })
end, { desc = "Format" })

-- if _G.Snacks then
--   Snacks.toggle.zoom():map("<C-w>m")
--   vim.keymap.set("n", "<c-\\>", function()
--     Snacks.terminal(nil, { win = { style = "float", border = "rounded" } })
--   end, { desc = "Terminal (Root dir)" })
--   vim.keymap.set("t", "<c-\\>", "<cmd>close<cr>", { desc = "Hide Terminal" })
-- end

function Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then
    file_pattern = file_pattern:gsub(".", ".*%0") .. ".*"
  end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
end
vim.opt.findfunc = "v:lua.Fd"
