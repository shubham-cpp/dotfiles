return {
  'akinsho/nvim-toggleterm.lua',
  version = '*',
  keys = { '<leader>tf', '<leader>tl', '<C-\\>' },
  opts = {
    direction = 'float',
    open_mapping = [[<c-\>]],
    -- shell = vim.o.shell == '/bin/fish' and '/bin/zsh' or vim.o.shell,
  },
  config = function(_, opts)
    local Terminal = require('toggleterm.terminal').Terminal
    local toggleterm = require 'toggleterm'
    local lazygit = Terminal:new({
      cmd = 'lazygit',
      direction = 'float',
      float_opts = { border = 'curved' },
    })

    local function lazygit_toggle()
      local directory = vim.fs.root(vim.fn.resolve(vim.fn.expand '%:p:h'), '.git')
        or vim.uv.cwd()
        or vim.fn.expand '%:p:h'
      lazygit.dir = directory
      lazygit:toggle()
    end

    local file_manager = Terminal:new({
      cmd = 'yazi',
      direction = 'float',
      dir = vim.fs.root(0, '.git') or vim.uv.cwd(),
      hidden = false,
      float_opts = { border = 'curved' },
    })

    local function file_manager_toggle()
      local directory = vim.fs.root(vim.fn.resolve(vim.fn.expand '%:p:h'), '.git')
        or vim.uv.cwd()
        or vim.fn.expand '%:p:h'
      file_manager.dir = directory
      file_manager:toggle()
    end

    toggleterm.setup(opts)
    vim.keymap.set('n', '<Space>tl', lazygit_toggle, { desc = '[T]erminal [L]azygit' })
    vim.keymap.set('n', '<Space>tf', file_manager_toggle, { desc = '[T]erminal [F]ile manager' })
  end,
}
