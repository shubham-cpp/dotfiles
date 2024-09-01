---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'svelte-language-server',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'svelte',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        svelte = {
          settings = {
            typescript = {
              updateImportsOnFileMove = { enabled = 'always' },
              inlayHints = {
                parameterNames = { enabled = 'all' },
                parameterTypes = { enabled = true },
                variableTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                enumMemberValues = { enabled = true },
              },
            },
            javascript = {
              updateImportsOnFileMove = { enabled = 'always' },
              inlayHints = {
                parameterNames = { enabled = 'literals' },
                parameterTypes = { enabled = true },
                variableTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                enumMemberValues = { enabled = true },
              },
            },
            svelte = {
              ['enable-ts-plugin'] = true,
              plugin = { svelte = { defaultScriptLanguage = 'typescript' } },
            },
          },
        },
      },
    },
  },
}
