local Terminal = require('toggleterm.terminal').Terminal
local toggleterm = require 'toggleterm'
local lazygit = Terminal:new({
  cmd = 'lazygit',
  dir = 'git_dir',
  direction = 'float',
  shell = vim.o.shell == '/bin/fish' and '/bin/zsh' or vim.o.shell,
  float_opts = { border = 'curved' },
})

local function lazygit_toggle()
  lazygit:toggle()
end

local file_manager = Terminal:new({
  cmd = 'yazi',
  shell = vim.o.shell == '/bin/fish' and '/bin/zsh' or vim.o.shell,
  direction = 'float',
  float_opts = { border = 'curved' },
})

local function file_manager_toggle()
  file_manager:toggle()
end

toggleterm.setup({
  shell = vim.o.shell == '/bin/fish' and '/bin/zsh' or vim.o.shell,
  direction = 'float',
  open_mapping = [[<c-\>]],
})
vim.keymap.set('n', '<Space>tl', lazygit_toggle, { desc = '[T]erminal [L]azygit' })
vim.keymap.set('n', '<Space>tf', file_manager_toggle, { desc = '[T]erminal [F]ile manager' })
