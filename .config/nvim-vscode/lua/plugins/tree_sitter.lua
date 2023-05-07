local config = {
	'nvim-treesitter/nvim-treesitter',
	version = false, -- last release is way too old and doesn't work on Windows
	build = ':TSUpdate',
	event = { 'BufReadPost', 'BufNewFile' },
	dependencies = {
		'windwp/nvim-ts-autotag',
	},
	enabled = not vim.g.vscode,
	config = function()
		require('nvim-treesitter.configs').setup({
			autotag = { enable = true },
			highlight = { enable = true },
			indent = { enable = true },
		})
	end,
}
return config
