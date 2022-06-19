local M = {}
local cmd = vim.cmd
function map(mode, lhs, rhs, opts)
	opts.buffer = opts.buffer == nil and true or opts.buffer
	opts.noremap = opts.noremap == nil and true or opts.noremap
	opts.silent = opts.silent == nil and true or opts.silent
	vim.keymap.set(mode, lhs, rhs, opts)
end

function M.on_attach(client, bufnr)
	cmd([[ command! -bar LspFormat :lua vim.lsp.buf.format({async=false})<cr> ]])
	cmd([[ command! -bar LspDiagnostics :lua vim.lsp.diagnostic.show_line_diagnostics()<CR> ]])
	cmd([[ command! -bar LspDefination :lua vim.lsp.buf.definition()<CR> ]])
	cmd([[ command! -bar LspDeclarations :lua vim.lsp.buf.declaration()<CR> ]])
	cmd([[ command! -bar LspImplementaions :lua vim.lsp.buf.implementation()<CR> ]])
	cmd([[ command! -bar LspReferences :lua vim.lsp.buf.references()<CR>]])
	cmd([[ command! -bar LspRename :lua vim.lsp.buf.rename()<CR> ]])
	cmd([[ command! -bar LspCodeAction :lua vim.lsp.buf.code_action()<CR> ]])
	cmd([[ command! -bar LspHover :lua vim.lsp.buf.hover()<CR> ]])

	map('n', 'gd', ':LspDefination<CR>', { buffer = bufnr })
	map('n', 'gD', ':LspDeclaration<CR>', { buffer = bufnr })
	map('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
	map('i', '<C-s>', 'vim.lsp.buf.signature_help', { buffer = bufnr })
	map('n', 'gi', vim.lsp.buf.implementation, { buffer = bufnr })
	-- map('n', 'gr', ':LspReferences<CR>',{buffer=bufnr})
	map('n', 'gr', '<cmd>FzfLua lsp_references<CR>', { buffer = bufnr })
	map('n', 'gt', vim.lsp.buf.type_definition, { buffer = bufnr })
	map('n', 'gw', vim.lsp.buf.document_symbol, { buffer = bufnr })
	map('n', 'gW', vim.lsp.buf.workspace_symbol, { buffer = bufnr })
	map('n', 'gac', ':LspCodeAction<CR>', { buffer = bufnr })

	-- map('n', '<leader>ci', vim.lsp.buf.incoming_calls, { buffer = bufnr })
	-- map('n', '<leader>co', vim.lsp.buf.outgoing_calls, { buffer = bufnr })
	-- map('n', '<leader>ca', '<cmd>FzfLua lsp_code_actions<CR>', { buffer = bufnr })

	map('n', '<F2>', ':LspRename<CR>', { buffer = bufnr })
	-- map('n', '<leader>cq', vim.lsp.diagnostic.set_loclist, { buffer = bufnr })

	map('n', '<Space>=', vim.lsp.buf.format, { buffer = bufnr })
	if client.server_capabilities.documentRangeFormattingProvider then
		map('v', '<Space>=', vim.lsp.buf.range_formatting, { buffer = bufnr })
	end
	-- For some reason prettier doesn't support svelte by default
	if client.name ~= 'svelte' then
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end
	if client.name == 'clangd' then
		client.offset_encoding = 'utf-8'
		client.offset_encodings = 'utf-8'
	end

	map('n', 'gl', vim.diagnostic.open_float, { buffer = bufnr })
	-- map("n", "ge", vim.diagnostic.open_float, { buffer = bufnr })
	map('n', '[g', vim.diagnostic.goto_prev, { buffer = bufnr })
	map('n', ']g', vim.diagnostic.goto_next, { buffer = bufnr })
	if client.server_capabilities.documentHighlightProvider then
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
	print(string.format('LSP attached (%s)', client.name))
end

return M
