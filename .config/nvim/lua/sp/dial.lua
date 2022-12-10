local map = function(mode, lhs, rhs)
  require('sp.helper').map(mode, lhs, rhs, { noremap = false, silent = false })
end
local augend = require 'dial.augend'
require('dial.config').augends:register_group({
  default = {
    augend.integer.alias.decimal,
    augend.constant.alias.bool,
    augend.integer.alias.hex,
    augend.date.alias['%Y/%m/%d'],
  },
  typescript = {
    augend.integer.alias.decimal,
    augend.integer.alias.hex,
    augend.constant.new({ elements = { 'let', 'const' } }),
  },
  visual = {
    augend.integer.alias.decimal,
    augend.integer.alias.hex,
    augend.integer.alias.hex,
    augend.date.alias['%Y/%m/%d'],
    augend.constant.alias.alpha,
    augend.constant.alias.Alpha,
  },
})
map('n', '<C-a>', '<Plug>(dial-increment)')
map('n', '<C-x>', '<Plug>(dial-decrement)')
map('v', '<C-a>', '<Plug>(dial-increment)')
map('v', '<C-x>', '<Plug>(dial-decrement)')
map('v', 'g<C-a>', 'g<Plug>(dial-increment)')
map('v', 'g<C-x>', 'g<Plug>(dial-decrement)')
