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
  },
}
