---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'elixir-ls',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'elixir',
        'erlang',
        'eex',
        'heex',
      })
      return opts
    end,
  },
  {
    'elixir-tools/elixir-tools.nvim',
    version = '*',
    lazy = true,
    dependencies = { 'nvim-lua/plenary.nvim' },
  },
  {
    'AstroNvim/astrolsp',
    opts = {
      handlers = {
        elixir_ls = function(_, opts)
          local elixir = require 'elixir'
          local elixirls = require 'elixir.elixirls'
          opts.capabilities = require('plugins.config.util').get_lsp_capabilities(opts.capabilities)

          elixir.setup({
            nextls = { enable = false },
            credo = { enable = true },
            elixirls = {
              enable = true,
              capabilities = opts.capabilities,
              settings = elixirls.settings({
                dialyzerEnabled = true,
                enableTestLenses = true,
              }),
              on_attach = function(client, bufnr)
                opts.on_attach(client, bufnr)
                vim.keymap.set('n', '<space>lp', ':ElixirFromPipe<cr>', { buffer = bufnr, noremap = true })
                vim.keymap.set('n', '<space>lP', ':ElixirToPipe<cr>', { buffer = bufnr, noremap = true })
                vim.keymap.set('v', '<space>lm', ':ElixirExpandMacro<cr>', { buffer = bufnr, noremap = true })
              end,
            },
          })
        end,
      },
    },
  },
}
