local config = {
  'svermeulen/vim-subversive',
  keys = {
    { 'x', '<Plug>(SubversiveSubstitute)', silent = true, noremap = false },
    { 'xx', '<Plug>(SubversiveSubstituteLine)', silent = true, noremap = false },
    { 'X', '<Plug>(SubversiveSubstituteToEndOfLine)', silent = true, noremap = false },
  },
}
return config
