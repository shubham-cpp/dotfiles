local map = function(opts)
  local mode = opts.mode or "v"
  vim.keymap.set(mode, opts.lhs, opts.rhs, { noremap = false, silent = false, desc = opts.desc })
end
local augend = require("dial.augend")
require("dial.config").augends:register_group({
  default = {
    augend.integer.alias.decimal,
    augend.constant.alias.bool,
    augend.integer.alias.hex,
    augend.date.alias["%Y/%m/%d"],
  },
  typescript = {
    augend.integer.alias.decimal,
    augend.integer.alias.hex,
    augend.constant.new({ elements = { "let", "const" } }),
  },
  visual = {
    augend.integer.alias.decimal,
    augend.integer.alias.hex,
    augend.integer.alias.hex,
    augend.date.alias["%Y/%m/%d"],
    augend.constant.alias.alpha,
    augend.constant.alias.Alpha,
  },
})
map({ mode = "n", lhs = "<C-a>", rhs = "<Plug>(dial-increment)" })
map({ mode = "n", lhs = "<C-x>", rhs = "<Plug>(dial-decrement)" })
map({ lhs = "<C-a>", rhs = "<Plug>(dial-increment)" })
map({ lhs = "<C-x>", rhs = "<Plug>(dial-decrement)" })
map({ lhs = "g<C-a>", rhs = "g<Plug>(dial-increment)" })
map({ lhs = "g<C-x>", rhs = "g<Plug>(dial-decrement)" })
