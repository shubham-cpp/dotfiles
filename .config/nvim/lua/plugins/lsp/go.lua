---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'gopls',
        'goimports',
        'golangci-lint',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'go',
        'gomod',
        'gosum',
        'gotmpl',
        'gowork',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        gopls = {
          settings = {
            gopls = {
              experimentalPostfixCompletions = true,
              analyses = {
                ST1003 = true,
                fillreturns = true,
                nilness = true,
                nonewvars = true,
                shadow = true,
                undeclaredname = true,
                unreachable = true,
                unusedparams = true,
                unusedwrite = true,
                useany = true,
              },
              hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                compositeLiteralTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
              },
              buildFlags = { '-tags', 'integration' },
              completeUnimported = true,
              diagnosticsDelay = '500ms',
              gofumpt = true,
              matcher = 'Fuzzy',
              semanticTokens = true,
              symbolMatcher = 'fuzzy',
              usePlaceholders = true,
              staticcheck = true,
              codelenses = {
                usePlaceholders = true,
                gc_details = true, -- Show a code lens toggling the display of gc's choices.
                generate = true, -- show the `go generate` lens.
                regenerate_cgo = true,
                test = true,
                tidy = true,
                upgrade_dependency = true,
                vendor = true,
              },
            },
          },
        },
      },
    },
  },
}
