return {
  'akinsho/nvim-toggleterm.lua',
  version = '*',
  keys = { '<leader>tt', '<leader>tf', '<leader>tg', '<C-\\>' },
  opts = {
    direction = 'float',
    open_mapping = [[<c-\>]],
  },
  config = function(_, opts)
    local Terminal = require('toggleterm.terminal').Terminal
    local toggleterm = require 'toggleterm'
    local lazygit = Terminal:new({
      cmd = 'lazygit',
      dir = 'git_dir',
      direction = 'float',
      float_opts = { border = 'curved' },
    })

    local function lazygit_toggle()
      lazygit:toggle()
    end

    local ranger = Terminal:new({
      cmd = 'lfv',
      direction = 'float',
      float_opts = { border = 'curved' },
    })

    local function ranger_toggle()
      ranger:toggle()
    end

    toggleterm.setup(opts)
    vim.keymap.set('n', '<Space>tg', lazygit_toggle, { desc = '[T]erminal Lazy[g]it' })
    vim.keymap.set('n', '<Space>tF', ranger_toggle, { desc = '[T]erminal [F]ile manager' })
  end,
}
