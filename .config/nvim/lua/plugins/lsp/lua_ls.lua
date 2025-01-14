---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'lua-language-server',
        'stylua',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'lua',
        'luadoc',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        lua_ls = {
          settings = {
            Lua = {
              diagnostics = {
                enable = true,
                globals = { 'vim', 'describe' },
                disable = { 'lowercase-global' },
              },
              workspace = {
                checkThirdParty = false,
                library = {
                  vim.env.VIMRUNTIME,
                },
              },
              runtime = { version = 'LuaJIT' },
              telemetry = { enable = false },
              completion = { callSnippet = 'Replace' },
              hint = { enable = true, arrayIndex = 'Disable' },
            },
          },
        },
      },
    },
  },
}
