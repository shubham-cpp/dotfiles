return {
  'mg979/vim-visual-multi',
  branch = 'master',
  version = false,
  init = function()
    vim.g.VM_maps = {}
    vim.g.VM_mouse_mappings = 1
    vim.g.VM_maps = {
      ['Find Under'] = '<M-d>',
      ['Find Subword Under'] = '<M-d>',
      ['Skip Region'] = '<C-x>',
      ['Select All'] = '<M-a>',
      ['Start Regex Search'] = '\\/',
    }
  end,
  keys = { { '<M-d>', mode = { 'n', 'v' } }, { '<M-a>', mode = { 'n', 'v' } } },
}
