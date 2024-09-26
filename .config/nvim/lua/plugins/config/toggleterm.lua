local Terminal = require('toggleterm.terminal').Terminal
local toggleterm = require 'toggleterm'
local lazygit = Terminal:new({
  cmd = 'lazygit',
  dir = 'git_dir',
  direction = 'float',
  -- shell = vim.o.shell == '/bin/fish' and '/bin/zsh' or vim.o.shell,
  float_opts = { border = 'curved' },
})

local function lazygit_toggle()
  lazygit:toggle()
end

toggleterm.setup({
  shell = vim.o.shell == '/bin/fish' and '/bin/zsh' or vim.o.shell,
  direction = 'float',
  open_mapping = [[<c-\>]],
  highlights = {
    FloatBorder = { link = 'FloatBorder' },
  },
  float_opts = {
    border = 'single',
  },
  -- highlights = {
  --   Normal = {
  --     guibg = '#080818',
  --   },
  --   NormalFloat = {
  --     guibg = '#080818',
  --   },
  -- },
})
vim.keymap.set('n', '<Space>tl', lazygit_toggle, { desc = '[T]erminal [L]azygit' })
