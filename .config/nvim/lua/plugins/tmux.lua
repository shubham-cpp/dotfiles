vim.keymap.set('n', '<C-h>', '<C-W>h')
vim.keymap.set('n', '<C-j>', '<C-W>j')
vim.keymap.set('n', '<C-k>', '<C-W>k')
vim.keymap.set('n', '<C-l>', '<C-W>l')

return {
  {
    'alexghergh/nvim-tmux-navigation',
    enabled = false,
    config = function()
      -- return require('tmux').setup()
      require('nvim-tmux-navigation').setup({})
    end,
    keys = {
      { '<C-h>', '<cmd>NvimTmuxNavigateLeft<CR>' },
      { '<C-j>', '<cmd>NvimTmuxNavigateDown<CR>' },
      { '<C-k>', '<cmd>NvimTmuxNavigateUp<CR>' },
      { '<C-l>', '<cmd>NvimTmuxNavigateRight<CR>' },
    },
  },
}
