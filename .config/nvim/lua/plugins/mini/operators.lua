---@type LazySpec
return {
  'echasnovski/mini.operators',
  version = '*',
  enabled = true,
  keys = {
    { 'g=', mode = { 'n', 'x' }, desc = 'Evalute' },
    { 'ge', mode = { 'n', 'x' }, desc = 'Exchange' },
    { 'gm', mode = { 'n', 'x' }, desc = 'Duplicate' },
    { 'x', mode = { 'n', 'x' }, desc = 'Replace with register' },
    { 'gS', mode = { 'n', 'x' }, desc = 'Sort' },
    'X',
  },
  opts = {
    -- Exchange text regions
    exchange = { prefix = 'ge' },
    replace = { prefix = 'x' },
    sort = { prefix = 'gS' },
  },
  config = function(_, opts)
    require('mini.operators').setup(opts)
    vim.keymap.set('n', 'X', 'x$', { desc = 'Replace to end of line', remap = true })
  end,
}
