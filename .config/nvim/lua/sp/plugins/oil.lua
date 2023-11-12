return {
  'stevearc/oil.nvim',
  keys = {
    { '\\', '<cmd>Oil<cr>', desc = 'Open parent directory' },
  },
  cmd = 'Oil',
  opts = {
    win_options = {
      cursorcolumn = false,
    },
    delete_to_trash = true,
  },
  -- Optional dependencies
  dependencies = { 'nvim-tree/nvim-web-devicons' },
}
