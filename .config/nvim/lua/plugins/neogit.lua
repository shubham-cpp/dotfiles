return {
  'NeogitOrg/neogit',
  cmd = 'Neogit',
  keys = {
    { '<leader>og', '<cmd>Neogit<cr>', desc = 'Neogit' },
    { '<leader>gn', '<cmd>Neogit<cr>', desc = 'Neogit' },
  },
  dependencies = {
    'nvim-lua/plenary.nvim', -- required
    { 'sindrets/diffview.nvim', cmd = 'DiffviewOpen', opts = {} }, -- optional - Diff integration
  },
  opts = {
    telescope_sorter = function()
      return require('telescope').extensions.fzf.native_fzf_sorter()
    end,
  },
}
