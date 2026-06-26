if type(vim.pack) ~= "table" then
	return
end

require("plugins.mini")
require("plugins.blink")
require("plugins.lsp")
require("plugins.conform")
require("plugins.nvim-lint")
require("plugins.treesitter")
require("plugins.multicursor")
require("plugins.color-highlight")
require("plugins.misc")
require("plugins.overseer")
