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
  end,
}
