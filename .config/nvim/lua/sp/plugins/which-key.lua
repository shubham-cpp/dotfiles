local util = require 'sp.util'
return {
  'folke/which-key.nvim',
  config = function()
    local wk = require 'which-key'
    wk.setup({
      plugins = {
        spelling = {
          enabled = true,
          suggestions = 20,
        },
        presets = {
          operators = false,
          motions = false,
          windows = true,
          nav = true,
          z = true,
          g = true,
        },
      },
      window = {
        border = 'single',
        position = 'bottom',
        margin = { 1, 0, 1, 0 },
        padding = { 2, 2, 2, 2 },
      },
      hidden = { '<silent>', '<cmd>', '<Cmd>', '<CR>', 'call', 'lua', '^:', '^ ', 'require' },
      triggers_blacklist = {
        i = { 'j', 'k' },
        v = { 'j', 'k' },
      },
    })
    wk.register({
      ['1'] = { '1gt', 'Goto tab 1' },
      ['2'] = { '2gt', 'Goto tab 2' },
      ['3'] = { '3gt', 'Goto tab 3' },
      ['4'] = { '4gt', 'Goto tab 4' },
      ['5'] = { '5gt', 'Goto tab 5' },
      ['6'] = { '6gt', 'Goto tab 6' },
      ['7'] = { '7gt', 'Goto tab 7' },
      ['8'] = { '8gt', 'Goto tab 8' },
      ['9'] = { '9gt', 'Goto tab 9' },
      b = {
        name = 'Buffers',
        a = { ':badd<space>', 'Add', silent = false },
        d = {
          name = 'Delete',
          l = { util.close_left, 'Close Left' },
          r = { util.close_right, 'Close Right' },
        },
        D = { '<cmd>bd<cr>', 'Delete' },
        c = {
          function()
            util.close_all(true, true)
          end,
          'Close All',
        },
        C = {
          function()
            util.close_all(false, true)
          end,
          'Close All(ALL)',
        },
        n = { '<cmd>bn<cr>', 'Next' },
        p = { '<cmd>bp<cr>', 'Prev' },
      },
      f = { name = 'Find', l = 'lsp' },
      g = { name = 'Git' },
      l = { name = 'LSP', r = 'Rename', a = 'Code Action' },
      n = { name = 'Neogen' },
      s = { name = 'Sessions' },
      o = { name = 'Open' },
      p = { name = 'Picker' },
      z = { name = 'ZK(Notes)' },
      t = {
        name = 'Toggle',
        f = '[T]erminal [F]ile manager',
        l = '[T]erminal Lazy[g]it',
      },
    }, { prefix = '<leader>' })
    wk.register({
      [']q'] = { '<cmd>cn<cr>', 'Quickfix Next ' },
      ['[q'] = { '<cmd>cprev<cr>', 'Quickfix Prev ' },
      [']b'] = { '<cmd>bnext<cr>', '[B]uffer Next ' },
      ['[b'] = { '<cmd>bprev<cr>', '[B]uffer Prev ' },
    })
    wk.register({
      g = {
        name = 'Lsp',
        e = 'Line Diagnostics',
        t = 'Goto type definition',
        d = 'Goto definition',
        a = { c = 'Code action' },
      },
    })
  end,
}
