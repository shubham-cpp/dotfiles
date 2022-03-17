local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")
local lsp_installer = require("nvim-lsp-installer")
local on_attach_common = require("config.lsp.utils").on_attach

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
	properties = { "documentation", "detail", "additionalTextEdits" },
}

local cmp_capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

lsp_installer.on_server_ready(function(server)
	local opts = {
		capabilities = cmp_capabilities,
		on_attach = on_attach_common,
		flags = { debounce_text_changes = 150 },
	}
	if server.name == "clangd" then -- {{{
		local clangd_capabilities = capabilities
		clangd_capabilities.textDocument.semanticHighlighting = true
		opts.capabilities = clangd_capabilities
		opts.cmd = {
			"clangd",
			"--background-index",
			"--pch-storage=memory",
			-- "--clang-tidy",
			"--suggest-missing-includes",
			"--cross-file-rename",
			-- "--completion-style=detailed",
			-- "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*",
		}
		opts.init_options = {
			clangdFileStatus = true,
			usePlaceholders = true,
			completeUnimported = true,
			semanticHighlighting = true,
		} -- }}}
	elseif server.name == "sumneko_lua" then -- {{{
		local runtime_path = vim.split(package.path, ";")
		table.insert(runtime_path, "lua/?.lua")
		table.insert(runtime_path, "lua/?/init.lua")

		opts.settings = {
			Lua = {
				runtime = { version = "LuaJIT", path = runtime_path },
				completion = { enable = true, callSnippet = "Both" },
				diagnostics = {
					enable = true,
					globals = { "vim", "describe" },
					disable = { "lowercase-global" },
				},
				workspace = {
					library = vim.api.nvim_get_runtime_file("", true),
					maxPreload = 2000,
					preloadFileSize = 1000,
				},
				telemetry = { enable = false },
			},
		} -- }}}
	elseif server.name == "jsonls" then -- {{{
		opts.settings = {
			json = {
				schemas = vim.list_extend({
					{
						name = "Vsnip snippets",
						description = "Extend vs code snippet completion for vsnip",
						fileMatch = { string.format("%s/vsnip/*.json", vim.fn.stdpath("config")) },
						url = "https://raw.githubusercontent.com/Yash-Singh1/vscode-snippets-json-schema/main/schema.json",
					},
				}, require("schemastore").json.schemas()),
			},
		} -- }}}
	elseif server.name == "yamlls" then -- {{{
		opts.settings = {
			yaml = {
				schemas = require("schemastore").json.schemas({
					select = {
						"docker-compose.yml",
						".yarnrc.yml",
						"GitHub Workflow",
						"GitHub Workflow Template Properties",
						"GitHub Action",
						"Azure Pipelines",
					},
				}),
			},
		} -- }}}
	elseif server.name == "tsserver" then -- {{{
		local ts_utils = require("nvim-lsp-ts-utils")
		---Create a nnoremap for buffer
		---@param bufnr number Buffer number
		---@param lhs string keys to map
		---@param rhs string command to execute with mapping
		local bnoremap = function(bufnr, lhs, rhs)
			vim.api.nvim_buf_set_keymap(bufnr, "n", lhs, rhs, { noremap = true, silent = true })
		end

		opts.init_options = ts_utils.init_options

		opts.on_attach = function(client, bufnr)
			client.resolved_capabilities.document_formatting = false
			client.resolved_capabilities.document_range_formatting = false

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
			bnoremap(bufnr, "gs", ":TSLspOrganize<CR>")
			bnoremap(bufnr, "gR", ":TSLspRenameFile<CR>")
			bnoremap(bufnr, "gi", ":TSLspImportAll<CR>")
		end -- }}}
	elseif server.name == "tailwindcss" then -- {{{
		local util = require("lspconfig.util")
		opts.root_dir = util.root_pattern(
			"tailwind.config.js",
			"tailwind.config.cjs",
			"tailwind.config.ts",
			"postcss.config.js",
			"postcss.config.cjs",
			"postcss.config.ts"
		)
		opts.settings = {
			tailwindCSS = {
				emmetCompletions = true,
			},
		}
		-- }}}
	elseif server.name == "vuels" then -- {{{
		opts.init_options = {
			validation = { templateProps = true },
			vetur = {
				format = {
					enable = false,
					defaultFormatter = {
						js = "none",
						ts = "none",
					},
					defaultFormatterOptions = {},
					scriptInitialIndent = false,
					styleInitialIndent = false,
				},
				experimental = { templateInterpolationService = true },
				completion = {
					autoImport = true,
					tagCasing = "kebab",
					useScaffoldSnippets = true,
				},
				useWorkspaceDependencies = true,
			},
		} -- }}}
	elseif server.name == "pyright" then -- {{{
		opts.settings = {
			python = {
				analysis = {
					autoImportCompletions = true,
					typeCheckingMode = "basic",
				},
				exclude = { "**/node_modules", "**/__pycache__" },
			},
		}
	end -- }}}
	server:setup(opts)
end)
lspconfig.gopls.setup({ -- {{{
	-- cmd = {"gopls", "serve"},
	cmd = { "gopls", "--remote=auto" },
	settings = { gopls = { analyses = { unusedparams = true }, staticcheck = true } },
	capabilties = cmp_capabilities,
	init_options = { usePlaceholders = true, completeUnimported = true },
	on_attach = on_attach_common,
	flags = { debounce_text_changes = 150 },
}) -- }}}

local util = require("lspconfig.util")

-- if not lspconfig.emmet_language_server then
configs.emmet_language_server = {
	default_config = {
		cmd = { "emmet-language-server", "--stdio" },
		filetypes = {
			"typescriptreact",
			"javascriptreact",
			"javascript",
			"javascript.jsx",
			"typescript.tsx",
		},
		root_dir = util.root_pattern("package.json", ".git"),
		settings = {},
	},
}
-- end
lspconfig.emmet_language_server.setup({
	capabilities = cmp_capabilities,
	flags = { debounce_text_changes = 150 },
})

local clangd_capabilities = capabilities
clangd_capabilities.textDocument.semanticHighlighting = true
lspconfig.clangd.setup({
	capabilities = clangd_capabilities,
	cmd = {
		"clangd",
		"--background-index",
		"--pch-storage=memory",
		"--clang-tidy",
		"--suggest-missing-includes",
		"--cross-file-rename",
		"--completion-style=detailed",
		-- "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*",
	},
	init_options = {
		clangdFileStatus = true,
		usePlaceholders = true,
		completeUnimported = true,
		semanticHighlighting = true,
	},
	flags = { debounce_text_changes = 150 },
})
