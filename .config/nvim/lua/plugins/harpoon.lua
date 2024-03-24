return {
	"ThePrimeagen/harpoon",
	branch = "harpoon2",
	dependencies = { "nvim-lua/plenary.nvim" },
	keys = function()
		local harpoon = require("harpoon")
		return {
			{
				"<leader>a",
				function()
					harpoon:list():append()
				end,
				desc = "Harpoon Append",
			},
			{
				"<C-S-P>",
				function()
					harpoon:list():prev()
				end,
				desc = "Harpoon Prev",
			},
			{
				"<C-S-N>",
				function()
					harpoon:list():next()
				end,
				desc = "Harpoon Next",
			},
			{
				"'1",
				function()
					harpoon:list():select(1)
				end,
				desc = "Harpoon Select 1",
			},
			{
				"'2",
				function()
					harpoon:list():select(2)
				end,
				desc = "Harpoon Select 2",
			},
			{
				"'3",
				function()
					harpoon:list():select(3)
				end,
				desc = "Harpoon Select 3",
			},
			{
				"'4",
				function()
					harpoon:list():select(4)
				end,
				desc = "Harpoon Select 4",
			},
			{
				"<C-e>",
				function()
					harpoon.ui:toggle_quick_menu(harpoon:list())
				end,
				desc = "Harpoon Select 4",
			},
		}
	end,
	opts = {},
}
