---@type LazySpec
return {
  'echasnovski/mini.align',
  keys = { 'ga', 'gA' },
  config = function()
    require('mini.align').setup({})
  end,
}
