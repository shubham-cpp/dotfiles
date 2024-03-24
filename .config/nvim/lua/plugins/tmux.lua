return {
	{
		"alexghergh/nvim-tmux-navigation",
		enabled = true,
		config = function()
			-- return require('tmux').setup()
			require("nvim-tmux-navigation").setup({})
		end,
		keys = {
			{ "<C-h>", "<cmd>NvimTmuxNavigateLeft<CR>" },
			{ "<C-j>", "<cmd>NvimTmuxNavigateDown<CR>" },
			{ "<C-k>", "<cmd>NvimTmuxNavigateUp<CR>" },
			{ "<C-l>", "<cmd>NvimTmuxNavigateRight<CR>" },
		},
	},
}
