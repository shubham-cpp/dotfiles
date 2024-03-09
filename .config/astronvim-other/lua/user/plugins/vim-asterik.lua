return {
  'haya14busa/vim-asterisk',
  -- event = 'BufReadPost',
  init = function()
    vim.g['asterisk#keeppos'] = 1
  end,
  --   config = function()
  --     vim.cmd [[let g:asterisk#keeppos = 1
  -- map *   <Plug>(asterisk-*)
  -- map #   <Plug>(asterisk-#)
  -- map g*  <Plug>(asterisk-g*)
  -- map g#  <Plug>(asterisk-g#)
  -- map z*  <Plug>(asterisk-z*)
  -- map gz* <Plug>(asterisk-gz*)
  -- map z#  <Plug>(asterisk-z#)
  -- map gz# <Plug>(asterisk-gz#)]]
  --   end,
  keys = {
    {
      '*',
      '<Plug>(asterisk-*)',
      desc = 'Search current word',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      '#',
      '<Plug>(asterisk-#)',
      desc = 'Search current word backward',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      'g*',
      '<Plug>(asterisk-g*)',
      desc = 'Search current word',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      'g#',
      '<Plug>(asterisk-g#)',
      desc = 'Search current word backward',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      'z*',
      '<Plug>(asterisk-z*)',
      desc = 'Search current word',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      'z#',
      '<Plug>(asterisk-z#)',
      desc = 'Search current word backward',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      'gz*',
      '<Plug>(asterisk-gz*)',
      desc = 'Search current word',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
    {
      'gz#',
      '<Plug>(asterisk-gz#)',
      desc = 'Search current word backward',
      mode = { 'n', 'v' },
      noremap = false,
      silent = true,
    },
  },
}
