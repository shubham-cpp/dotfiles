---@type LazySpec
return {
  'svermeulen/vim-subversive',
  enabled = false,
  keys = {
    { 'x', '<Plug>(SubversiveSubstitute)', noremap = false },
    { 'X', '<Plug>(SubversiveSubstituteToEndOfLine)', noremap = false },
    { 'xx', '<Plug>(SubversiveSubstituteLine)', noremap = false },
  },
}
