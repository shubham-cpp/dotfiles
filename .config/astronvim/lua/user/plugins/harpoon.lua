return {
	"ThePrimeagen/harpoon",
	branch = "harpoon2",
	dependencies = { "nvim-lua/plenary.nvim" },
	keys = function()
		local harpoon = require("harpoon")
		local list = harpoon:list()
		return {
			{
				"<leader>a",
				function()
					list:append()
				end,
				desc = "Harpoon Append",
			},
			{
				"<C-S-P>",
				function()
					list:prev()
				end,
				desc = "Harpoon Prev",
			},
			{
				"<C-S-N>",
				function()
					list:next()
				end,
				desc = "Harpoon Next",
			},
			{
				"'1",
				function()
					list:select(1)
				end,
				desc = "Harpoon Select 1",
			},
			{
				"'2",
				function()
					list:select(2)
				end,
				desc = "Harpoon Select 2",
			},
			{
				"'3",
				function()
					list:select(3)
				end,
				desc = "Harpoon Select 3",
			},
			{
				"'4",
				function()
					list:select(4)
				end,
				desc = "Harpoon Select 4",
			},
			{
				"<C-e>",
				function()
					harpoon.ui:toggle_quick_menu(require("harpoon"):list())
				end,
				desc = "Harpoon Select 4",
			},
		}
	end,
	opts = {},
}
