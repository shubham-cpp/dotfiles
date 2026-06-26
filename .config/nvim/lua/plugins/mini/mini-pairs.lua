require("mini.pairs").setup({
	modes = { insert = true, command = false, terminal = false },
	mappings = {
		["("] = { action = "open", pair = "()", neigh_pattern = "^[^\\][^%w_]" },
		["["] = { action = "open", pair = "[]", neigh_pattern = "^[^\\][^%w_]" },
		["{"] = { action = "open", pair = "{}", neigh_pattern = "^[^\\][^%w_]" },

		[")"] = { action = "close", pair = "()", neigh_pattern = "^[^\\][^%w_]" },
		["]"] = { action = "close", pair = "[]", neigh_pattern = "^[^\\][^%w_]" },
		["}"] = { action = "close", pair = "{}", neigh_pattern = "^[^\\][^%w_]" },

		['"'] = { action = "closeopen", pair = '""', neigh_pattern = "^[^%w_\\][^%w_]", register = { cr = false } },
		["'"] = { action = "closeopen", pair = "''", neigh_pattern = "^[^%w_\\][^%w_]", register = { cr = false } },
		["`"] = { action = "closeopen", pair = "``", neigh_pattern = "^[^%w_\\][^%w_]", register = { cr = false } },
	},
})
