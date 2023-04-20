return {
  { 'lewis6991/gitsigns.nvim', opts = {}, after = 'nvim-treesitter' },
  {
    'akinsho/git-conflict.nvim',
    after = 'gitsigns.nvim',
    opts = {},
    version = '*',
    -- config = function()
    --   vim.api.nvim_create_autocommand('User', {
    --     group = vim.api.nvim_create_augroup('sp_git', { clear = true }),
    --     pattern = 'GitConflictDetected',
    --     callback = function()
    --       vim.notify('Conflict detected in ' .. vim.fn.expand '<afile>')
    --       vim.keymap.set('n', 'cww', function()
    --         engage.conflict_buster()
    --         create_buffer_local_mappings()
    --       end)
    --     end,
    --   })
    -- end
  },
  {
    'TimUntersberger/neogit',
    cmd = { 'Neogit' },
    keys = { { '<leader>gg', '<cmd>Neogit<cr>', desc = 'Neogit' } },
    opts = {
      integrations = {
        diffview = true,
      },
    },
    dependencies = { { 'sindrets/diffview.nvim', opts = {} } },
  },
}
