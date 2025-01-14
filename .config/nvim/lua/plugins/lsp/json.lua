---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'json-lsp',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'json',
        'jsonc',
        'json5',
      })
      return opts
    end,
  },
  { 'b0o/schemastore.nvim', lazy = true, version = false },
  {
    'AstroNvim/astrolsp',
    opts = {
      handlers = {
        jsonls = function(server, opts)
          opts.capabilities = require('my_config.util').get_lsp_capabilities(opts.capabilities)
          opts.on_new_config = function(new_config)
            new_config.settings.json.schemas = new_config.settings.json.schemas or {}
            vim.list_extend(new_config.settings.json.schemas, require('schemastore').json.schemas())
          end
          opts.settings = {
            json = {
              validate = { enable = true },
              format = { enable = false },
            },
          }
          require('lspconfig')[server].setup(opts)
        end,
      },
    },
  },
}
