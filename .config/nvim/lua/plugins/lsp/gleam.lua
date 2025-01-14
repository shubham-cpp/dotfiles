---@type LazySpec
return {
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'gleam',
      })
      return opts
    end,
  },
  { 'AstroNvim/astrolsp', opts = { servers = { 'gleam' } } },
}
