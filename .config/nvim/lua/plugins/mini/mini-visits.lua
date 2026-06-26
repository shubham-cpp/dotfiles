--- {{{ Mini visits
require("mini.visits").setup({})

local visit_marks = require("config.visit_marks")
visit_marks.setup({})

vim.keymap.set("n", "<Leader>va", visit_marks.toggle, { desc = "Toggle File" })
vim.keymap.set("n", "<Leader>vv", visit_marks.toggle_window, { desc = "Toggle List" })
vim.keymap.set("n", "<Leader>vj", visit_marks.jump_input, { desc = "Jump Index" })
for index = 1, 9 do
	vim.keymap.set("n", "<Leader>v" .. index, function()
		visit_marks.jump(index)
	end, { desc = "Jump " .. index })
	vim.keymap.set("n", "<LocalLeader>" .. index, function()
		visit_marks.jump(index)
	end, { desc = "Jump " .. index })
end
--- }}}
