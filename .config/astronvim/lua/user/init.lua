return {
  lsp = {
    setup_handlers = {
      tsserver = function(_, opts)
        require('typescript-tools').setup({
          on_attach = function(client, bufnr)
            opts.on_attach(client, bufnr)
            vim.keymap.set('n', 'go', '<cmd>TSToolsOrganizeImports<cr>', { buffer = bufnr })
            vim.keymap.set('n', 'gD', '<cmd>TSToolsGoToSourceDefinition<cr>', { buffer = bufnr })
            vim.keymap.set('n', 'gR', '<cmd>TSToolsFileReferences<cr>', { buffer = bufnr })
            vim.keymap.set('n', '<F2>', '<cmd>TSToolsRenameFile<space>', { buffer = bufnr, silent = false })
          end,
          settings = {
            tsserver_file_preferences = {
              includeInlayParameterNameHints = 'all',
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
              '@styled/typescript-styled-plugin',
              -- or for older TypeScript versions
              -- "typescript-styled-plugin",
            },
          },
        })
      end,
      zk = function() end,
    },
  },
}
