---@type LazySpec
return {
  'echasnovski/mini.move',
  version = '*',
  keys = {
    { '<', mode = { 'n', 'v' } },
    { '>', mode = { 'n', 'v' } },
    { 'J', mode = 'v' },
    { 'K', mode = 'v' },
  },
  opts = {
    mappings = {
      -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
      left = '<',
      right = '>',
      down = 'J',
      up = 'K',

      -- Move current line in Normal mode
      line_left = '<',
      line_right = '>',
      line_down = '<M-j>',
      line_up = '<M-k>',
    },
  },
  config = function(_, opts)
    require('mini.move').setup(opts)
  end,
}
