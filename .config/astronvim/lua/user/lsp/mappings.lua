return {
  n = {
    ['<leader>='] = {
      function()
        vim.lsp.buf.format({ async = false })
      end,
      desc = 'Format File',
    },
    ['<leader>ff'] = {
      '<cmd>Telescope find_files hidden=true<cr>',
      desc = 'Find Files(hidden)',
    },
    ['gs'] = {
      function()
        vim.lsp.buf.code_action({ context = { only = { 'source.removeUnused' } }, apply = true })
        vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' } }, apply = true })
      end,
      desc = 'Organize Imports and Remove Unused',
    },
  },
}
