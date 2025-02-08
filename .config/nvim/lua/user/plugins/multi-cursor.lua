---@type LazySpec
return {
  {
    'mg979/vim-visual-multi',
    branch = 'master',
    version = false,
    enabled = false,
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
  },
  {
    'brenton-leighton/multiple-cursors.nvim',
    version = '*',
    enabled = false,
    cmd = {
      'MultipleCursorsAddDown',
      'MultipleCursorsAddUp',
      'MultipleCursorsAddMatches',
      'MultipleCursorsAddJumpNextMatch',
      'MultipleCursorsJumpNextMatch',
      'MultipleCursorsAddJumpPrevMatch',
      'MultipleCursorsJumpPrevMatch',
    },
    opts = {},
    keys = {
      { '<leader><down>', '<Cmd>MultipleCursorsAddDown<CR>', mode = { 'n', 'x' }, desc = 'Add cursor and move down' },
      { '<leader><up>', '<Cmd>MultipleCursorsAddUp<CR>', mode = { 'n', 'x' }, desc = 'Add cursor and move up' },

      { '<C-LeftMouse>', '<Cmd>MultipleCursorsMouseAddDelete<CR>', mode = { 'n', 'i' }, desc = 'Add or remove cursor' },
      { '<Leader>ma', '<Cmd>MultipleCursorsAddMatches<CR>', mode = { 'n', 'x' }, desc = 'Add cursors to cword' },
      {
        '<Leader>mA',
        '<Cmd>MultipleCursorsAddMatchesV<CR>',
        mode = { 'n', 'x' },
        desc = '+cursors to cword in prev area',
      },
      {
        '<Leader>mn',
        '<Cmd>MultipleCursorsAddJumpNextMatch<CR>',
        mode = { 'n', 'x' },
        desc = '+cursor & jump to next cword',
      },
      { '<Leader>mN', '<Cmd>MultipleCursorsJumpNextMatch<CR>', mode = { 'n', 'x' }, desc = 'Jump to next cword' },
      { '<Leader>ml', '<Cmd>MultipleCursorsLock<CR>', mode = { 'n', 'x' }, desc = 'Lock virtual cursors' },
    },
  },
  {
    'smoka7/multicursors.nvim',
    dependencies = {
      'nvimtools/hydra.nvim',
    },
    opts = {},
    cmd = { 'MCstart', 'MCvisual', 'MCclear', 'MCpattern', 'MCvisualPattern', 'MCunderCursor' },
    keys = {
      {
        mode = { 'v', 'n' },
        '<Leader>m',
        '<cmd>MCstart<cr>',
        desc = 'Create a selection for selected text or word under the cursor',
      },
    },
  },
}
