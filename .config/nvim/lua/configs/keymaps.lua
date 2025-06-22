local map = vim.keymap.set

map("n", "<localleader>e", ':e <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = "Edit in same dir" })
map("n", "<localleader>t", ':tabe <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = "Edit in same dir(Tab)" })
map(
  "n",
  "<localleader>v",
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir(Split)" }
)

map("", "0", "^", { desc = "Goto First", silent = false })
map("n", ",w", "<cmd>w!<cr>", { desc = "Save" })
map("n", ",W", "<cmd>noautocmd w!<cr>", { desc = "Save" })

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "<Down>", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })
map({ "n", "x" }, "<Up>", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })

-- Move to window using the <ctrl> hjkl keys
map("n", "<C-h>", "<C-w>h", { desc = "Go to Left Window", remap = true })
map("n", "<C-j>", "<C-w>j", { desc = "Go to Lower Window", remap = true })
map("n", "<C-k>", "<C-w>k", { desc = "Go to Upper Window", remap = true })
map("n", "<C-l>", "<C-w>l", { desc = "Go to Right Window", remap = true })

map("n", "[b", "<cmd>bprevious<cr>", { desc = "Prev Buffer" })
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next Buffer" })
map("n", "[t", "<cmd>tprevious<cr>", { desc = "Prev Tab" })
map("n", "]t", "<cmd>tnext<cr>", { desc = "Next Tab" })
map("n", "[q", vim.cmd.cprev, { desc = "Previous Quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next Quickfix" })

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
map("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
map("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
map("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
map("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

-- Add undo break-points
map("i", ",", ",<c-g>u")
map("i", ".", ".<c-g>u")
map("i", ";", ";<c-g>u")

-- commenting
map("n", "gco", "o<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>", { desc = "Add Comment Below" })
map("n", "gcO", "O<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>", { desc = "Add Comment Above" })

-- Diagnostics
map("n", "gl", vim.diagnostic.open_float, { desc = "Line Diagnostics" })

map("n", "]d", function()
  vim.diagnostic.jump { count = 1, float = true }
end, { desc = "Next Diagnostic" })
map("n", "[d", function()
  vim.diagnostic.jump { count = -1, float = true }
end, { desc = "Prev Diagnostic" })

map("n", "]e", function()
  vim.diagnostic.jump { count = 1, float = true, severity = vim.diagnostic.severity.ERROR }
end, { desc = "Next Diagnostic" })
map("n", "[e", function()
  vim.diagnostic.jump { count = -1, float = true, severity = vim.diagnostic.severity.ERROR }
end, { desc = "Prev Diagnostic" })

map("n", "]w", function()
  vim.diagnostic.jump { count = 1, float = true, severity = vim.diagnostic.severity.WARN }
end, { desc = "Next Diagnostic" })
map("n", "[w", function()
  vim.diagnostic.jump { count = -1, float = true, severity = vim.diagnostic.severity.WARN }
end, { desc = "Prev Diagnostic" })

-- Pasting in visual mode doesn't copy
map("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })
map("n", "dl", '"_dl')
map("v", "D", '"_D')
map({ "n", "v" }, "c", '"_c')
map("n", "C", '"_C')

for i = 1, 9 do
  map("n", "<leader>" .. i, i .. "gt", { desc = "Goto Tab " .. i })
end
