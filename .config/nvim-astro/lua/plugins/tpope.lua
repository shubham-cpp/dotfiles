---@type LazySpec
return {
  { 'tpope/vim-repeat', event = 'VeryLazy' },
  {
    'tpope/vim-abolish',
    keys = { 'cr' },
    cmd = { 'Abolish', 'Subvert', 'S' },
  },
  {
    'tpope/vim-dispatch',
    cmd = { 'Dispatch', 'Make', 'Focus', 'Start', 'FocusDispatch', 'Spawn', 'Copen', 'AbortDispatch' },
  },
}
