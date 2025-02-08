---@type LazySpec
return {
  {
    'AstroNvim/astrolsp',
    ---@type AstroLSPConfig
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
  { 'nvim-treesitter', opts = { ensure_installed = { 'go', 'gowork', 'gomod', 'gosum', 'gotmpl' } } },
  {
    'mason.nvim',
    opts = {
      ensure_installed = { 'delve', 'gopls', 'golangci-lint', 'goimports', 'gofumpt' },
    },
  },
  { 'nvim-lint', opts = {
    linters_by_ft = {
      go = { 'golangcilint' },
    },
  } },
}
