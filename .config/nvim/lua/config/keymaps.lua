local map = vim.keymap.set

map("n", "0", "^", { desc = "Goto Beginning" })
map("n", ",w", "<cmd>w!<cr>", { desc = "File: Save" })
map("n", ",W", "<cmd>noautocmd w!<cr>", { desc = "File: Save Without Autocmds" })

map({ "n", "x" }, "j", function()
	return vim.v.count == 0 and "gj" or "j"
end, { desc = "Move Down", expr = true, silent = true })

map({ "n", "x" }, "k", function()
	return vim.v.count == 0 and "gk" or "k"
end, { desc = "Move Up", expr = true, silent = true })

-- map("n", "<C-h>", "<C-w><C-h>", { desc = "Win: Go Left" })
-- map("n", "<C-j>", "<C-w><C-j>", { desc = "Win: Go Down" })
-- map("n", "<C-k>", "<C-w><C-k>", { desc = "Win: Go Up" })
-- map("n", "<C-l>", "<C-w><C-l>", { desc = "Win: Go Right" })

map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Win: Increase Height" })
map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Win: Decrease Height" })
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Win: Decrease Width" })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Win: Increase Width" })

-- map("n", "<leader>bb", "<C-^>", { desc = "Buffer: Alternate" })
-- map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Buffer: Delete" })

map("n", "<Esc>", "<cmd>nohlsearch<cr><Esc>", { desc = "Search: Clear Highlight" })
map("n", "n", "nzzzv", { desc = "Search: Next" })
map("n", "N", "Nzzzv", { desc = "Search: Previous" })

map("n", "J", "mzJ`z", { desc = "Edit: Join Lines" })

map("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })
map("n", "dl", '"_dl')
map("v", "D", '"_D')
map({ "n", "v" }, "c", '"_c')
map("n", "C", '"_C')

map("n", "<localleader>e", ':e <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = "Edit in same dir" })
map("n", "<localleader>t", ':tabe <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = "Edit in same dir(Tab)" })
map(
	"n",
	"<localleader>v",
	':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
	{ silent = false, desc = "Edit in same dir(Vsplit)" }
)

local function quickfix_open()
	for _, win in ipairs(vim.fn.getwininfo()) do
		if win.quickfix == 1 and win.loclist == 0 then
			return true
		end
	end
	return false
end

local function loclist_open()
	for _, win in ipairs(vim.fn.getwininfo()) do
		if win.quickfix == 1 and win.loclist == 1 then
			return true
		end
	end
	return false
end

map("n", "<leader>oq", function()
	vim.cmd(quickfix_open() and "cclose" or "copen")
end, { desc = "Quickfix: Toggle" })

map("n", "<leader>ol", function()
	vim.cmd(loclist_open() and "lclose" or "lopen")
end, { desc = "LocList: Toggle" })

map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line" })
map("n", "gl", vim.diagnostic.open_float, { desc = "Line" })
map("n", "<leader>cq", vim.diagnostic.setqflist, { desc = "Quickfix" })
map("n", "<leader>cl", vim.diagnostic.setloclist, { desc = "Location List" })
map("n", "]e", function()
	vim.diagnostic.jump({ count = 1, severity = vim.diagnostic.severity.ERROR })
end, { desc = "Diagnostic: Next Error" })
map("n", "[e", function()
	vim.diagnostic.jump({ count = -1, severity = vim.diagnostic.severity.ERROR })
end, { desc = "Diagnostic: Previous Error" })

map("t", "<C-]>", [[<C-\><C-n>]], { desc = "Terminal: Normal Mode" })
map("n", "<leader>ou", function()
	vim.cmd.packadd("nvim.undotree")
	vim.cmd("Undotree")
end, { desc = "UndoTree" })

map("n", "<leader>ot", function()
	vim.cmd("vnew | te")
end, { desc = "Term: vertical" })
map("n", "<leader>oT", function()
	vim.cmd("tabnew | te")
end, { desc = "Term: tab" })

for i = 1, 9 do
	map("n", "<leader>" .. i, i .. "gt", { desc = "Tab: " .. i })
end

require("config.better_window_navigation").setup()
