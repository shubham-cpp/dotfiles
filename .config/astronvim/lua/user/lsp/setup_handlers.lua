local map = function(lhs, rhs, opts)
  opts = opts or {}
  opts.noremap = true
  opts.silent = true
  opts.buffer = opts.buffer or true
  if type(rhs) == 'string' then
    rhs = '<cmd>' .. rhs .. '<cr>'
  end
  vim.keymap.set('n', lhs, rhs, opts)
end
return {
  tsserver = function(_, opts)
    require('typescript-tools').setup({
      on_attach = function(client, bufnr)
        opts.on_attach(client, bufnr)
        map('go', 'TSToolsOrganizeImports', { buffer = bufnr })
        map('gD', 'TSToolsGoToSourceDefinition', { buffer = bufnr })
        map('gR', 'TSToolsFileReferences', { buffer = bufnr })
        map('<leader>lR', 'TSToolsRenameFile', { buffer = bufnr, desc = 'Rename File' })
      end,
      --- Making sure denols & typescript server dont conflict with each other
      root_dir = require('lspconfig.util').root_pattern('package.json', 'tsconfig.json'),
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
          -- 'typescript-styled-plugin',
        },
      },
    })
  end,
}
