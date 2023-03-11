return {
  root_dir = require('lspconfig.util').root_pattern('deno.json', 'deno.jsonc'),
  settings = {
    deno = {
      enable = true,
      lint = true,
      codeLens = { references = true, test = true, implementations = true, referencesAllFunctions = false },
      inlayHints = {
        enumMemberValues = { enabled = true },
        variableTypes = { enabled = true, suppressWhenArgumentMatchesName = true },
        parameterNames = { enabled = 'all', suppressWhenArgumentMatchesName = true },
        parameterTypes = { enabled = true },
        functionLikeReturnTypes = { enabled = true },
        propertyDeclarationTypes = { enabled = true },
      },
      suggest = {
        completeFunctionCalls = true,
        autoImports = true,
        imports = {
          hosts = {
            ['https://crux.land'] = true,
            ['https://deno.land'] = true,
            ['https://x.nest.land'] = true,
          },
        },
      },
    },
  },
}
