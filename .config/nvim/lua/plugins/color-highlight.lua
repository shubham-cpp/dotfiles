local u = require("config.utils")

vim.pack.add({ u.gh("brenoprata10/nvim-highlight-colors") })
require("nvim-highlight-colors").setup({
	---Render style
	---@type 'background'|'foreground'|'virtual'
	render = "background",
	enable_tailwind = true,
})
