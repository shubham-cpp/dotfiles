---@type LazySpec
return {
  'nmac427/guess-indent.nvim',
  event = 'BufRead',
  config = function()
    require('guess-indent').setup({})
  end,
}
