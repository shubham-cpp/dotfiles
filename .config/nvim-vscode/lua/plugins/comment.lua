local config = {
  'tpope/vim-commentary',
  keys = {
    'gc',
    {'gc',noremap=false,silent=false,mode={'o','x'}},
  },
}

return config
