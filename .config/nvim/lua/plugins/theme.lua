return {
	{
		"rose-pine/neovim",
		event = "VimEnter",
		enabled = false,
		name = "rose-pine",
		config = function()
			require("rose-pine").setup({
				dim_inactive_windows = true,
				styles = { transparency = false },
			})
			vim.cmd("colorscheme rose-pine-main")
			-- vim.cmd("colorscheme rose-pine-moon")
			-- vim.cmd("colorscheme rose-pine-dawn")
		end,
	},
	{
		"rebelot/kanagawa.nvim",
		event = "VimEnter",
		enabled = true,
		config = function()
			require("kanagawa").setup({
				compile = true,
				transparent = true, -- do not set background color
				dimInactive = true,
				theme = "wave", -- Load "wave" | "dragon" | "lotus"
			})
			vim.cmd("colorscheme kanagawa-wave")
			-- vim.cmd("colorscheme kanagawa-dragon")
			-- vim.cmd("colorscheme kanagawa-lotus")
		end,
	},
	{
		"rmehri01/onenord.nvim",
		enabled = false,
		event = "VimEnter",
		opts = {
			fade_nc = true,
			disable = {
				background = true,
			},
			styles = {
				comments = "italic",
				strings = "NONE",
				keywords = "NONE",
				functions = "bold",
				variables = "NONE",
				diagnostics = "underline",
			},
			custom_highlights = {
				QuickScopePrimary = { fg = "#dfbb78", bg = "#505050", style = "underline,bold" },
				QuickScopeSecondary = {
					fg = "#61afef",
					bg = "#505050",
					style = "underline,bold",
				},
				PmenuSel = { bg = "#61afef", fg = "#24253b" },
			},
		},
		config = function(_, opts)
			require("onenord").setup(opts)
			vim.cmd("colorscheme onenord")
		end,
	},
}