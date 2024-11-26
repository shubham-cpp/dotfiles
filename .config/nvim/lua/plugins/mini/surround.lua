---@type LazySpec
return {
  'echasnovski/mini.surround',
  version = '*',
  enabled = false,
  keys = {
    { 'sa', mode = { 'n', 'v' }, desc = 'Surround add' },
    { 'sd', mode = { 'n' }, desc = 'Surround delete' },
    { 'sr', mode = { 'n' }, desc = 'Surround replace' },
    { 'sf', mode = { 'n' }, desc = 'Surround find' },
    { 'sh', mode = { 'n' }, desc = 'Surround highlight' },
  },
  opts = {
    search_method = 'cover_or_prev',
    n_lines = 200,
  },
  config = function(_, opts)
    require('mini.surround').setup(opts)
  end,
}
