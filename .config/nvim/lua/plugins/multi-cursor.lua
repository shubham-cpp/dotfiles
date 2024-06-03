return {
  {
    'mg979/vim-visual-multi',
    branch = 'master',
    enabled = true,
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
    version = '*', -- Use the latest tagged version
    enabled = false,
    opts = {},
    cmd = {
      'MultipleCursorsAddDown',
      'MultipleCursorsAddUp',
      'MultipleCursorsMouseAddDelete',
      'MultipleCursorsAddMatches',
      'MultipleCursorsAddMatchesV',
      'MultipleCursorsAddJumpNextMatch',
      'MultipleCursorsJumpNextMatch',
      'MultipleCursorsLock',
    },
    keys = {
      { '<C-Down>', '<Cmd>MultipleCursorsAddDown<CR>', desc = 'Add cursor down', mode = { 'n', 'x' } },
      { '<C-Up>', '<Cmd>MultipleCursorsAddUp<CR>', desc = 'Add cursor up', mode = { 'n', 'x' } },
      {
        '<C-LeftMouse>',
        '<Cmd>MultipleCursorsMouseAddDelete<CR>',
        desc = 'Add cursor with mouse',
        mode = { 'n', 'x' },
      },
      { '<leader>ca', '<Cmd>MultipleCursorsAddMatches<CR>', desc = 'Add cursor matches', mode = { 'n', 'x' } },
      {
        '<leader>cA',
        '<Cmd>MultipleCursorsAddMatchesV<CR>',
        desc = 'Add cursor matches in previous visual area',
        mode = { 'n', 'x' },
      },
      {
        '<leader>cj',
        '<Cmd>MultipleCursorsAddJumpNextMatch<CR>',
        desc = 'Add cursor and jump to next match',
        mode = { 'n', 'x' },
      },
      {
        '<leader>cJ',
        '<Cmd>MultipleCursorsJumpNextMatch<CR>',
        desc = 'Move cursor to next match',
        mode = { 'n', 'x' },
      },
      { '<leader>cl', '<Cmd>MultipleCursorsLock<CR>', desc = 'Lock virtual cursors', mode = { 'n', 'x' } },
    },
  },
}
