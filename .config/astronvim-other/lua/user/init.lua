local util = require("user.lsp.util")

return {
	lsp = {
		config = {
			-- hls = { cmd = { util.xdg_data_bin() .. "/haskell-language-server-9.2.5", "--lsp" } },
			html = { cmd = { util.bun_path() .. "/vscode-html-language-server", "--stdio" } },
			cssls = { cmd = { util.bun_path() .. "/vscode-css-language-server", "--stdio" } },
			jsonls = { cmd = { util.bun_path() .. "/vscode-json-language-server", "--stdio" } },
			eslint = { cmd = { util.bun_path() .. "/vscode-eslint-language-server", "--stdio" } },
			volar = { cmd = { util.bun_path() .. "/vue-language-server", "--stdio" } },
			bashls = { cmd = { util.bun_path() .. "/bash-language-server", "start" } },
			-- awk_ls = { cmd = { util.bun_path() .. '/awk-language-server' } },
			dockerls = { cmd = { util.bun_path() .. "/docker-langserver", "--stdio" } },
			svelte = { cmd = { util.bun_path() .. "/svelteserver", "--stdio" } },
			-- emmet_ls = { cmd = { util.bun_path() .. '/emmet-ls', '--stdio' } },
			emmet_language_server = { cmd = { util.bun_path() .. "/emmet-language-server", "--stdio" } },
			vimls = { cmd = { util.bun_path() .. "/vim-language-server", "--stdio" } },
			astro = { cmd = { util.bun_path() .. "/astro-ls", "--stdio" } },
			prismals = { cmd = { util.bun_path() .. "/prisma-language-server", "--stdio" } },
			docker_compose_language_service = { cmd = { util.bun_path() .. "/docker-compose-langserver", "--stdio" } },
		},
		setup_handlers = {
			tsserver = function(_, opts)
				require("typescript-tools").setup({
					on_attach = function(client, bufnr)
						opts.on_attach(client, bufnr)
						vim.keymap.set("n", "go", "<cmd>TSToolsOrganizeImports<cr>", { buffer = bufnr })
						vim.keymap.set("n", "gD", "<cmd>TSToolsGoToSourceDefinition<cr>", { buffer = bufnr })
						vim.keymap.set("n", "gR", "<cmd>TSToolsFileReferences<cr>", { buffer = bufnr })
						vim.keymap.set("n", "<F2>", "<cmd>TSToolsRenameFile<space>", { buffer = bufnr, silent = false })
					end,
					settings = {
						tsserver_file_preferences = {
							includeInlayParameterNameHints = "all",
							includeCompletionsForModuleExports = true,
							includeInlayParameterNameHintsWhenArgumentMatchesName = false,
							includeInlayFunctionParameterTypeHints = true,
							includeInlayVariableTypeHints = true,
							includeInlayVariableTypeHintsWhenTypeMatchesName = false,
							includeInlayPropertyDeclarationTypeHints = true,
							includeInlayFunctionLikeReturnTypeHints = true,
							includeInlayEnumMemberValueHints = true,
						},
						tsserver_plugins = {
							-- for TypeScript v4.9+
							"@styled/typescript-styled-plugin",
							-- or for older TypeScript versions
							-- "typescript-styled-plugin",
						},
					},
				})
			end,
		},
	},
}
