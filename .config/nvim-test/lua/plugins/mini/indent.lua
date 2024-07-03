---@type LazySpec
return {
  'echasnovski/mini.indentscope',
  version = false,
  enabled = true,
  event = 'BufReadPost',
  config = function()
    require('mini.indentscope').setup({
      -- Which character to use for drawing scope indicator
      symbol = '╎',
    })
  end,
}
