local config = {
  'junegunn/vim-easy-align',
  keys = {
    { 'ga', '<Plug>(EasyAlign)', silent = true, noremap = false, mode = { 'n', 'x' } },
  },
}
return config
