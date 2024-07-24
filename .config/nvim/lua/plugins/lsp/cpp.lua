---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'clangd',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'c',
        'cpp',
        'make',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        clangd = {
          capabilities = {
            offsetEncoding = { 'utf-16' },
            clangdInlayHintsProvider = true,
          },
          settings = {
            InlayHints = {
              Enabled = true,
              ParameterNames = true,
              DeducedTypes = true,
            },
          },
          cmd = {
            -- 'clangd',
            -- '--background-index',
            -- '--clang-tidy',
            -- '--suggest-missing-includes',
            -- '--header-insertion=iwyu',
            -- '--completion-style=detailed',
            -- '--function-arg-placeholders',
            -- '--pch-storage=memory',
            -- '--fallback-style=llvm',
            'clangd',
            '--background-index',
            '--clang-tidy',
            '--suggest-missing-includes',
            '--header-insertion-decorators',
            '--all-scopes-completion',
            '--cross-file-rename',
            '--log=info',
            '--completion-style=detailed',
            -- "--enable-config", -- clangd 11+ supports reading from .clangd configuration file
            '--offset-encoding=utf-16',
            '--header-insertion=never',
          },
        },
      },
    },
  },
}
