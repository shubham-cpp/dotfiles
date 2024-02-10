return {
	"folke/flash.nvim",
	enabled = false,
	keys = function()
		local flash = require("flash")
		return {
			{
				"s",
				flash.jump,
				mode = { "n", "x", "o" },
				desc = "Flash",
			},
			{
				"S",
				flash.treesitter,
				mode = { "n", "o" },
				desc = "Flash Treesitter",
			},
			{
				"r",
				flash.remote,
				mode = "o",
				desc = "Remote Flash",
			},
			{
				"R",
				flash.treesitter_search,
				mode = { "o", "x" },
				desc = "Treesitter Search",
			},
			{
				"<c-s>",
				flash.toggle,
				mode = { "c" },
				desc = "Toggle Flash Search",
			},
		}
	end,
	opts = {},
}
