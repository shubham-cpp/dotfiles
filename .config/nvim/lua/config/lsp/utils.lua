local M = {}
local cmd = vim.cmd
local map = require("helper").bmap

function M.on_attach(client)
	cmd([[ command! -bar LspFormat :lua vim.lsp.buf.formatting()<cr> ]])
	cmd([[ command! -bar LspDiagnostics :lua vim.lsp.diagnostic.show_line_diagnostics()<CR> ]])
	cmd([[ command! -bar LspDefination :lua vim.lsp.buf.definition()<CR> ]])
	cmd([[ command! -bar LspDeclarations :lua vim.lsp.buf.declaration()<CR> ]])
	cmd([[ command! -bar LspImplementaions :lua vim.lsp.buf.implementation()<CR> ]])
	cmd([[ command! -bar LspReferences :lua vim.lsp.buf.references()<CR>]])
	cmd([[ command! -bar LspRename :lua vim.lsp.buf.rename()<CR> ]])
	cmd([[ command! -bar LspCodeAction :lua vim.lsp.buf.code_action()<CR> ]])
	cmd([[ command! -bar LspHover :lua vim.lsp.buf.hover()<CR> ]])

	map("n", "gd", ":LspDefination<CR>")
	map("n", "gD", ":LspDeclaration<CR>")
	map("n", "K", vim.lsp.buf.hover)
	map("i", "<C-s>", vim.lsp.buf.signature_help)
	map("i", "<C-]>", vim.lsp.buf.signature_help)
	map("n", "gi", vim.lsp.buf.implementation)
	-- map('n', 'gr', ':LspReferences<CR>')
	map("n", "gr", "<cmd>FzfLua lsp_references<CR>")
	map("n", "gt", vim.lsp.buf.type_definition)
	map("n", "gw", vim.lsp.buf.document_symbol)
	map("n", "gW", vim.lsp.buf.workspace_symbol)
	map("n", "gac", ":LspCodeAction<CR>")

	-- map("n", "<leader>ci", vim.lsp.buf.incoming_calls)
	-- map("n", "<leader>co", vim.lsp.buf.outgoing_calls)
	-- map("n", "<leader>ca", ":LspCodeAction<CR>")
	-- map("n", "<leader>ca", "<cmd>FzfLua lsp_code_actions<CR>")

	map("n", "<F2>", ":LspRename<CR>")
	map("n", "gQ", vim.lsp.diagnostic.set_loclist)

	map("n", "<Space>=", vim.lsp.buf.formatting)
	if client.resolved_capabilities.document_range_formatting then
		map("v", "<Space>=", vim.lsp.buf.range_formatting)
	end
	-- For some reason prettier doesn't support svelte by default
	if client.name ~= "svelte" then
		client.resolved_capabilities.document_formatting = false
	end

	map("n", "J", function()
		vim.diagnostic.open_float(0, { scope = "line" })
	end)
	map("n", "ge", function()
		vim.diagnostic.open_float(0, { scope = "line" })
	end)
	map("n", "[g", function()
		vim.diagnostic.goto_prev({ float = { border = "single" } })
	end)
	map("n", "]g", function()
		vim.diagnostic.goto_next({ float = { border = "single" } })
	end)

	if client.resolved_capabilities.document_highlight then
		vim.api.nvim_exec(
			[[
		hi LspReferenceWrite cterm=bold ctermfg=red gui=bold guisp= guifg= guibg=#565575
		hi LspReferenceRead cterm=bold ctermfg=red gui=bold guisp= guifg= guibg=#565575
		hi LspReferenceText cterm=bold ctermfg=red gui=bold guisp= guifg= guibg=#565575
		hi LspDiagnosticsDefaultError cterm=bold ctermbg=red guifg=#ff3333
		hi LspDiagnosticsDefaultWarning cterm=bold ctermbg=red guifg=#e7ae05
		augroup lsp_document_highlight
		autocmd! * <buffer>
		autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
		" autocmd CursorHold <buffer> lua vim.diagnostic.open_float(0, {scope='line'})
		autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
		" autocmd CursorHoldI <buffer> silent! lua vim.lsp.buf.signature_help()
		augroup END
		]],
			false
		)
	end
	print(string.format("LSP attached (%s)", client.name))
end

return M
