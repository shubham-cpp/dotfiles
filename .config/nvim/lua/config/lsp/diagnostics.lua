vim.diagnostic.config({
	underline = true,
	update_in_insert = false,
	virtual_text = {
		spacing = 4,
		source = "always",
		-- severity = 'error'
		-- prefix = '👾',
	},
	signs = true,
	severity_sort = true,
	float = { show_header = true, source = "always" },
})
vim.fn.sign_define("DiagnosticSignError", { text = " ", texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = " ", texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = " ", texthl = "DiagnosticSignHint" })
