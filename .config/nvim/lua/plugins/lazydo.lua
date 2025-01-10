---@type LazySpec
return {
  'Dan7h3x/LazyDo',
  keys = { -- recommended keymap for easy toggle LazyDo in normal and insert modes (arbitrary)
    {
      '<F2>',
      '<CMD>LazyDoToggle<CR>',
      mode = { 'n', 'i' },
      desc = 'Toggle LazyDo',
    },
  },
  opts = {},
}
