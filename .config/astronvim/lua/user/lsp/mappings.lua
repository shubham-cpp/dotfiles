return {
	n = {
		["<leader>="] = {
			function()
				vim.lsp.buf.format({ async = false })
			end,
			desc = "Format File",
		},
		["<leader>ff"] = {
			"<cmd>Telescope find_files hidden=true<cr>",
			desc = "Find Files(hidden)",
		},
		["gs"] = {
			function()
				vim.lsp.buf.code_action({ context = { only = { "source.removeUnused" } }, apply = true })
				vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
			end,
			desc = "Organize Imports and Remove Unused",
		},
		["gw"] = {
			"<cmd>Telescope lsp_document_symbols<cr>",
			desc = "Document Symbol(LSP)",
		},
		["gW"] = {
			"<cmd>Telescope lsp_workspace_symbols<cr>",
			desc = "Workspace Symbol(LSP)",
		},
		["]e"] = {
			function()
				vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR })
			end,
			desc = "Next [E]rror",
		},
		["[e"] = {
			function()
				vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR })
			end,
			desc = "Prev [E]rror",
		},
		["]c"] = {
			function()
				if vim.wo.diff then
					return "]c"
				end
				vim.schedule(function()
					package.loaded.gitsigns.next_hunk()
				end)
				return "<Ignore>"
			end,
			expr = true,
			desc = "Next hunk([C]hange)",
		},

		["[c"] = {
			function()
				if vim.wo.diff then
					return "[c"
				end
				vim.schedule(function()
					package.loaded.gitsigns.prev_hunk()
				end)
				return "<Ignore>"
			end,
			expr = true,
			desc = "Previous hunk([C]hange)",
		},
	},
	i = {
		["<C-h>"] = {
			vim.lsp.buf.signature_help,
			desc = "Signature Help",
		},
	},
}
