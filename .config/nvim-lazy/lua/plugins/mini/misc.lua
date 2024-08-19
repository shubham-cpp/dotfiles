---@type LazySpec
return {
  'echasnovski/mini.misc',
  version = '*',
  event = 'BufWinEnter',
  keys = {
    {
      '<leader>wm',
      function()
        require('mini.misc').zoom()
      end,
      desc = 'Window zoom',
    },
    {
      '<C-w>m',
      function()
        require('mini.misc').zoom()
      end,
      desc = 'Window zoom',
    },
  },
  config = function()
    require('mini.misc').setup({})
    require('mini.misc').setup_restore_cursor()
  end,
}
