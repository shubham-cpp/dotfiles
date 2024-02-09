return {
  'mrcjkb/rustaceanvim',
  version = '^3', -- Recommended
  ft = { 'rust' },
  init = function()
    vim.g.rustaceanvim = {
      -- Plugin configuration
      -- tools = {},
      -- LSP configuration
      server = {
        on_attach = function(client, bufnr)
          require('sp.util').on_attach(client, bufnr)
          vim.keymap.set('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr, desc = 'Format Buffer(Rust)' })
          vim.keymap.set('n', 'J', '<cmd>RustLsp joinLines<cr>', { buffer = bufnr, desc = 'Rust JoinLines' })
          vim.keymap.set('n', 'gL', '<cmd>RustLsp explainError<cr>', { buffer = bufnr, desc = 'Explain errors' })
        end,
        settings = {
          -- rust-analyzer language server configuration
          ['rust-analyzer'] = {
            diagnostics = { enable = true },
            check = { features = 'all' },
            inlayHints = { closureCaptureHints = { enable = true } },
            lens = {
              references = {
                adt = { enable = true },
                enumVariant = { enable = true },
                method = { enable = true },
                trait = { enable = true },
              },
            },
            checkOnSave = {
              command = {
                'cargo',
                'clippy',
                '--workspace',
                '--message-format=json',
                '--all-targets',
                '--all-features',
              },
            },
            typing = { autoClosingAngleBrackets = { enable = true } },
          },
        },
      },
      -- DAP configuration
      -- dap = {},
    }
  end,
}
