return {
  'kylechui/nvim-surround',
  config = function()
    require('nvim-surround').setup({})
  end,
  keys = { 'ys', 'ds', 'cs', { 'S', mode = 'v' } },
}
