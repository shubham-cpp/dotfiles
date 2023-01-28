local map = function(opts)
  local mode = opts.mode or 'v'
  vim.keymap.set(
    mode,
    opts.lhs,
    opts.rhs,
    { noremap = false, silent = false, desc = opts.desc, buffer = opts.buffer ~= nil and opts.buffer or false }
  )
end
local augend = require 'dial.augend'
require('dial.config').augends:register_group({
  default = {
    augend.integer.alias.decimal,
    augend.constant.alias.bool,
    augend.integer.alias.hex,
    augend.date.alias['%Y/%m/%d'],
    augend.constant.new({
      elements = { '&&', '||' },
      word = false,
      cyclic = true,
    }),
  },
  typescript = {
    augend.integer.alias.decimal,
    augend.constant.alias.bool,
    augend.integer.alias.hex,
    augend.constant.new({ elements = { 'let', 'const' } }),
    augend.date.alias['%Y/%m/%d'],
    augend.constant.new({
      elements = { '&&', '||' },
      word = false,
      cyclic = true,
    }),
  },
  py_lua = {
    augend.integer.alias.decimal,
    augend.constant.alias.bool,
    augend.integer.alias.hex,
    augend.constant.new({
      elements = { 'and', 'or' },
      word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
      cyclic = true, -- "or" is incremented into "and".
    }),
    augend.date.alias['%Y/%m/%d'],
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
map({ mode = 'n', lhs = '<C-a>', rhs = '<Plug>(dial-increment)' })
map({ mode = 'n', lhs = '<C-x>', rhs = '<Plug>(dial-decrement)' })
map({ lhs = '<C-a>', rhs = '<Plug>(dial-increment)' })
map({ lhs = '<C-x>', rhs = '<Plug>(dial-decrement)' })
map({ lhs = 'g<C-a>', rhs = 'g<Plug>(dial-increment)' })
map({ lhs = 'g<C-x>', rhs = 'g<Plug>(dial-decrement)' })

local auGroupDial = vim.api.nvim_create_augroup('auGroupDialNvim', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'typescript', 'javascript', 'typescriptreact', 'javascriptreact', 'html', 'vue', 'svelte' },
  desc = 'Use typescript group of dial.nvim',
  callback = function()
    vim.keymap.set(
      { 'n', 'x' },
      '<C-a>',
      '"=typescript<CR><Plug>(dial-increment)',
      { buffer = true, noremap = false, silent = true }
    )
    vim.keymap.set(
      { 'n', 'x' },
      '<C-x>',
      '"=typescript<CR><Plug>(dial-decrement)',
      { buffer = true, noremap = false, silent = true }
    )
  end,
  group = auGroupDial,
})

vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'python', 'lua' },
  desc = 'Use typescript group of dial.nvim',
  callback = function()
    vim.keymap.set(
      { 'n', 'x' },
      '<C-a>',
      '"=py_lua<CR><Plug>(dial-increment)',
      { buffer = true, noremap = false, silent = true }
    )
    vim.keymap.set(
      { 'n', 'x' },
      '<C-x>',
      '"=py_lua<CR><Plug>(dial-decrement)',
      { buffer = true, noremap = false, silent = true }
    )
  end,
  group = auGroupDial,
})
