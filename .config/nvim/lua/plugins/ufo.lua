return {
  'kevinhwang91/nvim-ufo',
  dependencies = 'kevinhwang91/promise-async',
  event = 'BufRead',

  keys = function()
    local ufo = require 'ufo'
    return {
      { 'zR', ufo.openAllFolds, desc = 'Open Folds' },
      { 'zM', ufo.closeAllFolds, desc = 'Close Folds' },
    }
  end,
  config = function()
    local filetype_exclude = { 'help', 'alpha', 'dashboard', 'neo-tree', 'Trouble', 'lazy', 'mason' }
    vim.api.nvim_create_autocmd('FileType', {
      group = vim.api.nvim_create_augroup('local_detach_ufo', { clear = true }),
      desc = 'Disable ufo for these filetypes',
      pattern = filetype_exclude,
      callback = function()
        require('ufo').detach()
      end,
    })
    require('ufo').setup({})
  end,
}
