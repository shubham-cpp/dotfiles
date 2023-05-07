local config = {
  'monaqa/dial.nvim',
  version = '*',
  keys = {
    { '<C-a>', '<Plug>(dial-increment)', mode = { 'n', 'v' } },
    { '<C-x>', '<Plug>(dial-decrement)', mode = { 'n', 'v' } },
    { 'g<C-a>', 'g<Plug>(dial-increment)', mode = 'v', noremap = false },
    { 'g<C-x>', 'g<Plug>(dial-decrement)', mode = 'v', noremap = false },
  },
  config = function()
local augend = require("dial.augend")
require("dial.config").augends:register_group{
  default = {
    augend.integer.alias.decimal,   -- nonnegative decimal number (0, 1, 2, 3, ...)
    augend.constant.alias.bool,       -- nonnegative hex number  (0x01, 0x1a1f, etc.)
    augend.date.alias["%Y/%m/%d"],  -- date (2022/02/19, etc.)
  },

}
  end,
}
return config
