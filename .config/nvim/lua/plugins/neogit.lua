return {
  'NeogitOrg/neogit',
  cmd = 'Neogit',
  keys = {
    { '<leader>og', '<cmd>Neogit<cr>', desc = 'Neogit' },
  },
  dependencies = {
    'nvim-lua/plenary.nvim', -- required
    'sindrets/diffview.nvim', -- optional - Diff integration
    -- Only one of these is needed, not both.
    'nvim-telescope/telescope.nvim', -- optional
  },
  opts = {
    telescope_sorter = function()
      return require('telescope').extensions.fzf.native_fzf_sorter()
    end,
  },
}
