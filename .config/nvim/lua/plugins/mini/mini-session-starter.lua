---{{{ Mini Session
require("mini.sessions").setup({})
vim.keymap.set("n", "<leader>ql", function()
	MiniSessions.get_latest()
end, { desc = "Load last" })
vim.keymap.set("n", "<leader>qL", function()
	MiniSessions.select()
end, { desc = "List" })
vim.keymap.set("n", "<leader>qs", function()
	local ok, _ = pcall(MiniSessions.write)
	if not ok then
		vim.ui.input({ prompt = "Session Name = ", scope = "buffer" }, function(input)
			if vim.trim(input or "") == "" then
				return
			end
			MiniSessions.write(input)
		end)
	end
end, { desc = "Save" })
vim.keymap.set("n", "<leader>qd", function()
	MiniSessions.delete()
end, { desc = "Delete" })
vim.keymap.set("n", "<leader>qr", function()
	MiniSessions.restart()
end, { desc = "Restart" })
---}}}
---{{{ Mini starter
local starter = require("mini.starter")
local my_items = {
	starter.sections.builtin_actions(),
	starter.sections.sessions(9, true),
	{ name = "Recent Files", action = ":Pick oldfiles", section = "MiniPick" },
	{ name = "File Picker", action = ":Pick files", section = "MiniPick" },
	{ name = "Select Sessions", action = ":lua MiniSessions.select()", section = "MiniPick" },
	starter.sections.recent_files(5, true),
}
starter.setup({
	autoopen = true,
	evaluate_single = true,
	items = my_items,
	header = nil,
	footer = nil,
	content_hooks = {
		starter.gen_hook.adding_bullet(),
		starter.gen_hook.indexing("all", { "Builtin actions", "MiniPick" }),
		starter.gen_hook.padding(5, 2),
		starter.gen_hook.aligning("left", "top"),
	},
	query_updaters = [[abcdefghijklmnopqrstuvwxyz0123456789_-.]],
})
---}}}
