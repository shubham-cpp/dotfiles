---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'phpcbf',
        'phpcs',
        'php-cs-fixer',
        'blade-formatter',
        'phpactor',
        'php-debug-adapter',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      local parser_config = require('nvim-treesitter.parsers').get_parser_configs()

      parser_config.blade = {
        install_info = {
          url = 'https://github.com/EmranMR/tree-sitter-blade',
          files = { 'src/parser.c' },
          branch = 'main',
        },
        filetype = 'blade',
      }

      vim.filetype.add({
        pattern = {
          ['.*%.blade%.php'] = 'blade',
        },
      })
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'php',
        'phpdoc',
        'blade',
      })
      return opts
    end,
  },
}
