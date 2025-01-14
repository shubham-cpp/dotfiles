---@type LazySpec
return {
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'rust',
      })
      return opts
    end,
  },
  {
    'mrcjkb/rustaceanvim',
    enabled = false,
    version = '^4', -- Recommended
    lazy = false, -- This plugin is already lazy
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        rust_analyzer = {
          settings = {
            ['rust-analyzer'] = {
              imports = {
                granularity = {
                  group = 'module',
                },
                prefix = 'self',
              },
              cargo = {
                buildScripts = {
                  enable = true,
                },
              },
              procMacro = {
                enable = true,
              },
            },
          },
        },
      },
      servers = { 'rust_analyzer' },
    },
  },
}
