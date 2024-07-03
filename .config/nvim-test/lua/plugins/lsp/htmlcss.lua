---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'html-lsp',
        'css-lsp',
        'prettierd',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'html',
        'css',
        'scss',
        'robot',
        'http',
        'regex',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    ---@type AstroLSPConfig
    opts = function(_, opts)
      opts.config = vim.tbl_extend('force', opts.config or {}, {
        html = {},
        cssls = {
          settings = {
            css = {
              validate = true,
              completion = {
                triggerPropertyValueCompletion = true,
                completePropertyWithSemicolon = true,
              },
              lint = {
                compatibleVendorPrefixes = 'warn',
                duplicateProperties = 'warn',
                boxModel = 'warn',
                unknownVendorSpecificProperties = 'warn',
                float = 'error',
              },
            },
            scss = {
              validate = true,
              completion = {
                triggerPropertyValueCompletion = true,
                completePropertyWithSemicolon = true,
              },
              lint = {
                compatibleVendorPrefixes = 'warn',
                duplicateProperties = 'warn',
                boxModel = 'warn',
                unknownVendorSpecificProperties = 'warn',
                float = 'error',
              },
            },
          },
        },
      })
      return opts
    end,
  },
}
