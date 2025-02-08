return {
  'NeogitOrg/neogit',
  cmd = 'Neogit',
  keys = {
    { '<leader>gn', '<cmd>Neogit kind=floating<cr>', desc = 'Neogit' },
  },
  dependencies = {
    'nvim-lua/plenary.nvim', -- required
    { 'sindrets/diffview.nvim', cmd = 'DiffviewOpen', opts = {} }, -- optional - Diff integration
  },
  opts = {},
}
