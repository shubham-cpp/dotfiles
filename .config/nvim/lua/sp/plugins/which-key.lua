return {
  'folke/which-key.nvim',
  event = { 'BufReadPost', 'BufNewFile' },
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
        d = { '<cmd>bd<cr>', 'Delete' },
        n = { '<cmd>bn<cr>', 'Next' },
        p = { '<cmd>bp<cr>', 'Prev' },
      },
      f = {
        name = 'Find',
        l = 'lsp',
      },
      g = {
        name = 'Git',
      },
      l = {
        name = 'LSP',
      },
      n = {
        name = 'Neogen',
      },
      s = {
        name = 'Sessions',
      },
      t = {
        name = 'Toggle',
        f = '[T]erminal [F]ile manager',
        g = '[T]erminal Lazy[g]it',
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
        s = 'Switch',
        e = 'Line Diagnostics',
        t = 'Goto type definition',
        d = 'Goto definition',
        a = { c = 'Code action' },
      },
    })
  end,
}
