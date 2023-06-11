local config = {
  'danymat/neogen',
  dependencies = 'nvim-treesitter/nvim-treesitter',
  config = true,
  version = '*',
  keys = {
    { '<leader>nc', "<cmd>lua require('neogen').generate({ type = 'class' })<cr>", desc = '[N]eoGenerate [C]lass' },
    { '<leader>nf', "<cmd>lua require('neogen').generate({ type = 'func' })<cr>", desc = '[N]eoGenerate [F]unction' },
    { '<leader>nF', "<cmd>lua require('neogen').generate({ type = 'file' })<cr>", desc = '[N]eoGenerate [F]ile' },
    { '<leader>nt', "<cmd>lua require('neogen').generate({ type = 'type' })<cr>", desc = '[N]eoGenerate [T]ype' },
    { '<leader>nn', "<cmd>lua require('neogen').generate()<cr>", desc = '[N]eoGenerate' },
  },
}

return config
