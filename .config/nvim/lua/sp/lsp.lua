local cmd = vim.cmd
local ok_fzf, fzf = pcall(require, 'fzf-lua')
vim.diagnostic.config({
	underline = true,
	update_in_insert = false,
	virtual_text = {
		spacing = 4,
		source = 'always',
		-- severity = 'error'
		-- prefix = '👾',
	},
	signs = true,
	severity_sort = true,
	float = { show_header = true, source = 'always' },
})
vim.fn.sign_define('DiagnosticSignError', { text = ' ', texthl = 'DiagnosticSignError' })
vim.fn.sign_define('DiagnosticSignWarn', { text = ' ', texthl = 'DiagnosticSignWarn' })
vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
vim.fn.sign_define('DiagnosticSignHint', { text = ' ', texthl = 'DiagnosticSignHint' })
require('nvim-lsp-installer').setup({
	automatic_installation = true, -- automatically detect which servers to install (based on which servers are set up via lspconfig)
	ui = {
		icons = {
			server_installed = '✓',
			server_pending = '➜',
			server_uninstalled = '✗',
		},
	},
})

function map(mode, lhs, rhs, opts)
	opts.buffer = opts.buffer == nil and true or opts.buffer
	opts.noremap = opts.noremap == nil and true or opts.noremap
	opts.silent = opts.silent == nil and true or opts.silent
	vim.keymap.set(mode, lhs, rhs, opts)
end

function on_attach(client, bufnr)
	cmd([[ command! -bar LspFormat :lua vim.lsp.buf.format({async=false})<cr> ]])
	cmd([[ command! -bar LspDiagnostics :lua vim.lsp.diagnostic.show_line_diagnostics()<CR> ]])
	cmd([[ command! -bar LspDefination :lua vim.lsp.buf.definition()<CR> ]])
	cmd([[ command! -bar LspDeclarations :lua vim.lsp.buf.declaration()<CR> ]])
	cmd([[ command! -bar LspImplementaions :lua vim.lsp.buf.implementation()<CR> ]])
	cmd([[ command! -bar LspReferences :lua vim.lsp.buf.references()<CR>]])
	cmd([[ command! -bar LspRename :lua vim.lsp.buf.rename()<CR> ]])
	cmd([[ command! -bar LspCodeAction :lua vim.lsp.buf.code_action()<CR> ]])
	cmd([[ command! -bar LspHover :lua vim.lsp.buf.hover()<CR> ]])

	map('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr })
	map('n', 'gD', vim.lsp.buf.declaration, { buffer = bufnr })
	map('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
	map('i', '<C-s>', 'vim.lsp.buf.signature_help', { buffer = bufnr })
	map('n', 'gi', vim.lsp.buf.implementation, { buffer = bufnr })
	-- map('n', 'gr', ':LspReferences<CR>',{buffer=bufnr})
	if ok_fzf then
		map('n', 'gr', function()
			fzf.lsp_references({
				winopts = {
					preview = {
						layout = 'vertical',
						vertical = 'up:40%',
					},
				},
			})
		end, { buffer = bufnr })
	else
		map('n', 'gr', '<cmd>Telescope lsp_references<CR>', { buffer = bufnr })
		-- map('n', '<leader>ca', '<cmd>Telescope lsp_code_actions<CR>', { buffer = bufnr })
	end
	map('n', 'gt', vim.lsp.buf.type_definition, { buffer = bufnr })
	map('n', 'gw', vim.lsp.buf.document_symbol, { buffer = bufnr })
	map('n', 'gW', vim.lsp.buf.workspace_symbol, { buffer = bufnr })
	map('n', 'gac', vim.lsp.buf.code_action, { buffer = bufnr })

	-- map('n', '<leader>ci', vim.lsp.buf.incoming_calls, { buffer = bufnr })
	-- map('n', '<leader>co', vim.lsp.buf.outgoing_calls, { buffer = bufnr })

	map('n', '<F2>', vim.lsp.buf.rename, { buffer = bufnr })
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

local capabilities = vim.lsp.protocol.make_client_capabilities()

local cmp_capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
	properties = { 'documentation', 'detail', 'additionalTextEdits' },
}
local lspconfig = require('lspconfig')
local util = lspconfig.util
-- Lua {{{
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

lspconfig.sumneko_lua.setup({
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		Lua = {
			runtime = { version = 'LuaJIT', path = runtime_path },
			completion = { enable = true, callSnippet = 'Both' },
			diagnostics = {
				enable = true,
				globals = { 'vim', 'describe' },
				disable = { 'lowercase-global' },
			},
			workspace = {
				library = vim.api.nvim_get_runtime_file('', true),
				maxPreload = 2000,
				preloadFileSize = 1000,
			},
			hint = { enable = true, setType = true },
			telemetry = { enable = false },
		},
	},
}) -- }}}
-- C/C++ {{{
local clangd_capabilities = capabilities
clangd_capabilities.textDocument.semanticHighlighting = true
clangd_capabilities.offsetEncoding = 'utf-8'
lspconfig.clangd.setup({
	capabilities = clangd_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	cmd = {
		'clangd',
		'--background-index',
		'--pch-storage=memory',
		'--clang-tidy',
		'--suggest-missing-includes',
		'--cross-file-rename',
		'--completion-style=detailed',
	},
	init_options = {
		clangdFileStatus = true,
		usePlaceholders = true,
		completeUnimported = true,
		semanticHighlighting = true,
	},
})
-- }}}
lspconfig.jsonls.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		json = {
			schemas = vim.list_extend({
				{
					name = 'Vsnip snippets',
					description = 'Extend vs code snippet completion for vsnip',
					fileMatch = { string.format('%s/vsnip/*.json', vim.fn.stdpath('config')) },
					url = 'https://raw.githubusercontent.com/Yash-Singh1/vscode-snippets-json-schema/main/schema.json',
				},
			}, require('schemastore').json.schemas()),
		},
	},
}) -- }}}
lspconfig.yamlls.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		yaml = {
			schemas = require('schemastore').json.schemas({
				select = {
					'docker-compose.yml',
					'.yarnrc.yml',
					'GitHub Workflow',
					'GitHub Workflow Template Properties',
					'GitHub Action',
					'Azure Pipelines',
				},
			}),
		},
		redhat = { telemetry = { enabled = false } },
	},
}) -- }}}
lspconfig.html.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
}) -- }}}
lspconfig.cssls.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
}) -- }}}
lspconfig.pyright.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		python = {
			analysis = {
				autoImportCompletions = true,
				typeCheckingMode = 'basic',
			},
			exclude = { '**/node_modules', '**/__pycache__' },
		},
	},
}) -- }}}
lspconfig.gopls.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		gopls = {
			experimentalPostfixCompletions = true,
			analyses = { unusedparams = true, shadow = true },
			staticcheck = true,
		},
	},
}) -- }}}
lspconfig.emmet_ls.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	filetypes = {
		'typescriptreact',
		'javascriptreact',
		'javascript.jsx',
		'typescript.tsx',
		'html',
		'css',
		'sass',
		'scss',
		'less',
	},
	root_dir = util.root_pattern('package.json', '.git'),
	settings = {},
	single_file_support = true,
}) -- }}}
lspconfig.tailwindcss.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	filetypes = {
		'typescriptreact',
		'javascriptreact',
		'javascript.jsx',
		'typescript.tsx',
		'html',
		-- 'css',
		-- 'sass',
		-- 'scss',
		-- 'less',
	},
	root_dir = util.root_pattern(
		'tailwind.config.js',
		'tailwind.config.cjs',
		'tailwind.config.ts',
		'postcss.config.js',
		'postcss.config.cjs',
		'postcss.config.ts'
	),
	settings = {
		tailwindCSS = {
			emmetCompletions = true,
		},
	},
	single_file_support = true,
}) -- }}}
-- Typescrip, Javascript {{{
local ts_utils = require('nvim-lsp-ts-utils')
---Create a nnoremap for buffer
---@param bufnr number Buffer number
---@param lhs string keys to map
---@param rhs string command to execute with mapping
local bnoremap = function(bufnr, lhs, rhs)
	vim.keymap.set('n', lhs, rhs, { noremap = true, silent = true, buffer = bufnr })
end
lspconfig.tsserver.setup({
	capabilities = cmp_capabilities,
	settings = {
		javascript = {
			referencesCodeLens = { enabled = true, showOnAllFunctions = true },
			suggest = { completeFunctionCalls = true },
		},
		typescript = { updateImportsOnFileMove = { enabled = 'always' } },
	},
	on_attach = function(client, bufnr)
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false

		ts_utils.setup({
			enable_import_on_completion = true,
			import_all_scan_buffers = 100,
			update_imports_on_move = true,
			-- filter out dumb module warning
			filter_out_diagnostics_by_code = { 80001 },
			require_confirmation_on_move = false,
			watch_dir = nil,
		})

		-- required to fix code action ranges and filter diagnostics
		ts_utils.setup_client(client)
		on_attach(client)
		-- no default maps, so you may want to define some here
		bnoremap(bufnr, 'gs', ':TSLspOrganize<CR>')
		bnoremap(bufnr, 'gR', ':TSLspRenameFile<CR>')
		bnoremap(bufnr, 'gi', ':TSLspImportAll<CR>')
	end,
	flags = { debounce_text_changes = 150 },
	init_options = ts_utils.init_options,
}) -- }}}
lspconfig.svelte.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		svelte = { plugin = { svelte = { useNewTransformation = true } } },
	},
}) --}}}
lspconfig.volar.setup({ -- {{{
	capabilities = cmp_capabilities,
	on_attach = on_attach,
	flags = { debounce_text_changes = 150 },
	settings = {
		volar = { autoCompleteRefs = true },
	},
	init_options = {
		documentFeatures = {
			documentColor = true,
		},
	},
}) --}}}
