---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'nimlangserver',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'nim',
        'nim_format_string',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        nim_langserver = {
          settings = {
            nim = {
              nimsuggestPath = vim.fn.expand '~/.local/share/nim-2.0.4/bin/nimsuggest',
            },
          },
        },
      },
      servers = { 'nim_langserver' },
    },
  },
}
