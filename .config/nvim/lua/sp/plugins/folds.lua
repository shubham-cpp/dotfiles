return {
  'kevinhwang91/nvim-ufo',
  event = 'BufReadPost',
  dependencies = { 'kevinhwang91/promise-async' },
  keys = {
    {
      'zR',
      function()
        require('ufo').openAllFolds()
      end,
      desc = 'Ufo Open Folds',
    },
    {
      'zM',
      function()
        require('ufo').closeAllFolds()
      end,
      desc = 'Ufo Close Fold',
    },
  },
  config = function()
    require('ufo').setup({
      provider_selector = function()
        return {'lsp','indent'}
      end
    })
  end,
}
