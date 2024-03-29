return {
  'akinsho/nvim-toggleterm.lua',
  version = '*',
  cmd = { 'ToggleTerm', 'TermExec' },
  keys = { '<leader>tf', '<leader>tl', '<C-\\>' },
  opts = {
    direction = 'float',
    open_mapping = [[<c-\>]],
    on_create = function()
      vim.opt.foldcolumn = '0'
      vim.opt.signcolumn = 'no'
    end,
  },
  config = function(_, opts)
    local Terminal = require('toggleterm.terminal').Terminal
    local toggleterm = require 'toggleterm'
    local lazygit = Terminal:new({
      cmd = 'lazygit',
      dir = "git_dir",
      direction = 'float',
    })

    local function lazygit_toggle()
      lazygit:toggle()
    end

    local file_manager = Terminal:new({
      cmd = 'lfv',
      direction = 'float',
      float_opts = { border = 'curved' },
    })

    local function file_manager_toggle()
      file_manager:toggle()
    end

    toggleterm.setup(opts)
    vim.keymap.set('n', '<Space>tl', lazygit_toggle, { desc = '[T]erminal Lazy[g]it' })
    vim.keymap.set('n', '<Space>tf', file_manager_toggle, { desc = '[T]erminal [F]ile manager' })
  end,
}
