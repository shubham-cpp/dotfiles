---@type LazySpec
return {
  'neovim/nvim-lspconfig',
  event = 'BufReadPre',
  dependencies = {
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'AstroNvim/astrolsp',
  },
  config = function()
    -- require('mason-lspconfig').setup(opts)
    local diags = { Error = '', Warn = '', Info = '', Hint = '' }
    for sign, icon in pairs(diags) do
      vim.fn.sign_define('DiagnosticSign' .. sign, { text = icon, texthl = 'DiagnosticSign' .. sign })
    end
    -- set up servers configured with AstroLSP
    vim.tbl_map(require('astrolsp').lsp_setup, require('astrolsp').config.servers)
    local lspconfig = require 'lspconfig'
    local configs = require 'lspconfig.configs'
    -- Configure it
    configs.blade = {
      default_config = {
        -- Path to the executable: laravel-dev-generators
        cmd = { '/home/shubham/.local/bin/laravel-dev-tools', 'lsp' },
        filetypes = { 'blade' },
        root_dir = function(fname)
          return lspconfig.util.find_git_ancestor(fname)
        end,
        settings = {},
      },
    }
    -- Set it up
    -- lspconfig.blade.setup({
    -- Capabilities is specific to my setup.
    -- capabilities = capabilities
    --   on_attach = require('astrolsp').on_attach,
    -- })
  end,
}
