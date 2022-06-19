-- local ensure_installed = {
--     'gopls',
--     'cssls',
--     'html',
--     'jsonls',
--     'bashls',
--     'tsserver',
--     'sumneko_lua',
--     'pyright',
--     'svelte',
--     'vimls',
--     'yamlls',
--     'volar',
-- }
local lsp_installer = require('nvim-lsp-installer')
-- lsp_installer.setup({
--     ensure_installed = ensure_installed,
--     ui = {
--         icons = {
--             server_installed = '✓',
--             server_pending = '➜',
--             server_uninstalled = '✗',
--         },
--     },
-- })

local lspconfig = require('lspconfig')
local util = lspconfig.util
local on_attach_common = require('config.lsp.utils').on_attach

local capabilities = vim.lsp.protocol.make_client_capabilities()

local cmp_capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
	properties = { 'documentation', 'detail', 'additionalTextEdits' },
}

-- local opts = {
--     capabilities = cmp_capabilities,
--     on_attach = on_attach_common,
--     flags = { debounce_text_changes = 150 },
-- }

-- for _, server in ipairs(ensure_installed) do
--     if server == 'sumneko_lua' then -- {{{
--         local runtime_path = vim.split(package.path, ';')
--         table.insert(runtime_path, 'lua/?.lua')
--         table.insert(runtime_path, 'lua/?/init.lua')

--         opts.settings = {
--             Lua = {
--                 runtime = { version = 'LuaJIT', path = runtime_path },
--                 completion = { enable = true, callSnippet = 'Both' },
--                 diagnostics = {
--                     enable = true,
--                     globals = { 'vim', 'describe' },
--                     disable = { 'lowercase-global' },
--                 },
--                 workspace = {
--                     library = vim.api.nvim_get_runtime_file('', true),
--                     maxPreload = 2000,
--                     preloadFileSize = 1000,
--                 },
--                 telemetry = { enable = false },
--             },
--         } -- }}}
--     elseif server == 'clangd' then -- {{{
--         local clangd_capabilities = capabilities
--         clangd_capabilities.textDocument.semanticHighlighting = true
--         clangd_capabilities.offsetEncoding = 'utf-8'

--         opts.capabilities = nil
--         opts.capabilities = clangd_capabilities
--         opts.cmd = {
--             'clangd',
--             '--background-index',
--             '--pch-storage=memory',
--             '--clang-tidy',
--             '--suggest-missing-includes',
--             '--cross-file-rename',
--             '--completion-style=detailed',
--         }
--         opts.init_options = {
--             clangdFileStatus = true,
--             usePlaceholders = true,
--             completeUnimported = true,
--             semanticHighlighting = true,
--         }
--         -- }}}
--     elseif server == 'jsonls' then -- {{{
--         opts.settings = {
--             json = {
--                 schemas = vim.list_extend({
--                     {
--                         name = 'Vsnip snippets',
--                         description = 'Extend vs code snippet completion for vsnip',
--                         fileMatch = { string.format('%s/vsnip/*.json', vim.fn.stdpath('config')) },
--                         url = 'https://raw.githubusercontent.com/Yash-Singh1/vscode-snippets-json-schema/main/schema.json',
--                     },
--                 }, require('schemastore').json.schemas()),
--             },
--         } -- }}}
--     elseif server == 'yamlls' then -- {{{
--         opts.settings = {
--             yaml = {
--                 schemas = require('schemastore').json.schemas({
--                     select = {
--                         'docker-compose.yml',
--                         '.yarnrc.yml',
--                         'GitHub Workflow',
--                         'GitHub Workflow Template Properties',
--                         'GitHub Action',
--                         'Azure Pipelines',
--                     },
--                 }),
--             },
--         } -- }}}
--     elseif server == 'tsserver' then -- {{{
--         local ts_utils = require('nvim-lsp-ts-utils')
--         ---Create a nnoremap for buffer
--         ---@param bufnr number Buffer number
--         ---@param lhs string keys to map
--         ---@param rhs string command to execute with mapping
--         local bnoremap = function(bufnr, lhs, rhs)
--             vim.api.nvim_buf_set_keymap(bufnr, 'n', lhs, rhs, { noremap = true, silent = true })
--         end

--         opts.init_options = ts_utils.init_options

--         opts.on_attach = function(client, bufnr)
--             client.server_capabilities.documentFormattingProvider = false
--             client.server_capabilities.documentRangeFormattingProvider = false

--             ts_utils.setup({
--                 enable_import_on_completion = true,
--                 import_all_scan_buffers = 100,
--                 update_imports_on_move = true,
--                 -- filter out dumb module warning
--                 filter_out_diagnostics_by_code = { 80001 },
--                 require_confirmation_on_move = false,
--                 watch_dir = nil,
--             })

--             -- required to fix code action ranges and filter diagnostics
--             ts_utils.setup_client(client)
--             on_attach_common(client)
--             -- no default maps, so you may want to define some here
--             bnoremap(bufnr, 'gs', ':TSLspOrganize<CR>')
--             bnoremap(bufnr, 'gR', ':TSLspRenameFile<CR>')
--             bnoremap(bufnr, 'gi', ':TSLspImportAll<CR>')
--         end -- }}}
--     elseif server == 'tailwindcss' then -- {{{
--         local util = require('lspconfig.util')
--         opts.root_dir = util.root_pattern(
--             'tailwind.config.js',
--             'tailwind.config.cjs',
--             'tailwind.config.ts',
--             'postcss.config.js',
--             'postcss.config.cjs',
--             'postcss.config.ts'
--         )
--         opts.settings = {
--             tailwindCSS = {
--                 emmetCompletions = true,
--             },
--         }
--         -- }}}
--     elseif server == 'vuels' then -- {{{
--         opts.init_options = {
--             validation = { templateProps = true },
--             vetur = {
--                 format = {
--                     enable = false,
--                     defaultFormatter = {
--                         js = 'none',
--                         ts = 'none',
--                     },
--                     defaultFormatterOptions = {},
--                     scriptInitialIndent = false,
--                     styleInitialIndent = false,
--                 },
--                 experimental = { templateInterpolationService = true },
--                 completion = {
--                     autoImport = true,
--                     tagCasing = 'kebab',
--                     useScaffoldSnippets = true,
--                 },
--                 useWorkspaceDependencies = true,
--             },
--         } -- }}}
--     elseif server == 'gopls' then -- {{{
--         opts.settings = {
--             gopls = {
--                 experimentalPostfixCompletions = true,
--                 analyses = { unusedparams = true, shadow = true },
--                 staticcheck = true,
--             },
--         }
--         opts.init_options = { usePlaceholders = true, completeUnimported = true }
--         -- }}}
--     elseif server == 'pyright' then -- {{{
--         opts.settings = {
--             python = {
--                 analysis = {
--                     autoImportCompletions = true,
--                     typeCheckingMode = 'basic',
--                 },
--                 exclude = { '**/node_modules', '**/__pycache__' },
--             },
--         }
--     elseif server == 'emmet_ls' then
--         opts.filetypes = {
--             'typescriptreact',
--             'javascriptreact',
--             'javascript.jsx',
--             'typescript.tsx',
--             'html',
--             'css',
--             'sass',
--             'scss',
--             'less',
--         }
--         opts.root_dir = util.root_pattern('package.json', '.git')
--         opts.settings = {}
--         opts.single_file_support = true
--     end -- }}}
--     lspconfig[server].setup({ opts })
-- end
lsp_installer.on_server_ready(function(server)
	local opts = {
		capabilities = cmp_capabilities,
		on_attach = on_attach_common,
		flags = { debounce_text_changes = 150 },
	}
	if server.name == 'sumneko_lua' then -- {{{
		local runtime_path = vim.split(package.path, ';')
		table.insert(runtime_path, 'lua/?.lua')
		table.insert(runtime_path, 'lua/?/init.lua')

		opts.settings = {
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
				telemetry = { enable = false },
			},
		} -- }}}
	elseif server.name == 'clangd' then -- {{{
		local clangd_capabilities = capabilities
		clangd_capabilities.textDocument.semanticHighlighting = true
		clangd_capabilities.offsetEncoding = 'utf-8'

		opts.capabilities = nil
		opts.capabilities = clangd_capabilities
		opts.cmd = {
			'clangd',
			'--background-index',
			'--pch-storage=memory',
			'--clang-tidy',
			'--suggest-missing-includes',
			'--cross-file-rename',
			'--completion-style=detailed',
		}
		opts.init_options = {
			clangdFileStatus = true,
			usePlaceholders = true,
			completeUnimported = true,
			semanticHighlighting = true,
		}
		-- }}}
	elseif server.name == 'jsonls' then -- {{{
		opts.settings = {
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
		} -- }}}
	elseif server.name == 'yamlls' then -- {{{
		opts.settings = {
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
		} -- }}}
	elseif server.name == 'tsserver' then -- {{{
		local ts_utils = require('nvim-lsp-ts-utils')
		---Create a nnoremap for buffer
		---@param bufnr number Buffer number
		---@param lhs string keys to map
		---@param rhs string command to execute with mapping
		local bnoremap = function(bufnr, lhs, rhs)
			vim.api.nvim_buf_set_keymap(bufnr, 'n', lhs, rhs, { noremap = true, silent = true })
		end

		opts.init_options = ts_utils.init_options

		opts.on_attach = function(client, bufnr)
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
			on_attach_common(client)
			-- no default maps, so you may want to define some here
			bnoremap(bufnr, 'gs', ':TSLspOrganize<CR>')
			bnoremap(bufnr, 'gR', ':TSLspRenameFile<CR>')
			bnoremap(bufnr, 'gi', ':TSLspImportAll<CR>')
		end -- }}}
	elseif server.name == 'tailwindcss' then -- {{{
		local util = require('lspconfig.util')
		opts.root_dir = util.root_pattern(
			'tailwind.config.js',
			'tailwind.config.cjs',
			'tailwind.config.ts',
			'postcss.config.js',
			'postcss.config.cjs',
			'postcss.config.ts'
		)
		opts.settings = {
			tailwindCSS = {
				emmetCompletions = true,
			},
		}
		-- }}}
	elseif server.name == 'vuels' then -- {{{
		opts.init_options = {
			validation = { templateProps = true },
			vetur = {
				format = {
					enable = false,
					defaultFormatter = {
						js = 'none',
						ts = 'none',
					},
					defaultFormatterOptions = {},
					scriptInitialIndent = false,
					styleInitialIndent = false,
				},
				experimental = { templateInterpolationService = true },
				completion = {
					autoImport = true,
					tagCasing = 'kebab',
					useScaffoldSnippets = true,
				},
				useWorkspaceDependencies = true,
			},
		} -- }}}
	elseif server.name == 'gopls' then -- {{{
		-- 	cmd = { "gopls", "--remote=auto" },
		opts.settings = {
			gopls = {
				experimentalPostfixCompletions = true,
				analyses = { unusedparams = true, shadow = true },
				staticcheck = true,
			},
		}
		opts.init_options = { usePlaceholders = true, completeUnimported = true }
		-- }}}
	elseif server.name == 'pyright' then -- {{{
		opts.settings = {
			python = {
				analysis = {
					autoImportCompletions = true,
					typeCheckingMode = 'basic',
				},
				exclude = { '**/node_modules', '**/__pycache__' },
			},
		}
	elseif server.name == 'emmet_ls' then
		opts.filetypes = {
			'typescriptreact',
			'javascriptreact',
			'javascript.jsx',
			'typescript.tsx',
			'html',
			'css',
			'sass',
			'scss',
			'less',
		}
		opts.root_dir = util.root_pattern('package.json', '.git')
		opts.settings = {}
		opts.single_file_support = true
	end -- }}}
	server:setup(opts)
end)
-- lspconfig.ccls.setup({
-- 	capabilities = cmp_capabilities,
-- 	on_attach = on_attach_common,
-- 	flags = { debounce_text_changes = 150 },
-- })
