local config = {
  n = {
    ['<C-\\>'] = { '<Cmd>exe v:count1 . "ToggleTerm"<CR>', desc = 'Open terminal' },
    ['<leader>c'] = false,
    ['0'] = { '^' },
    ['<Esc>'] = { '<cmd>nohl<cr>', desc = 'No Highlight' },
    [',w'] = { '<cmd>w!<cr>', desc = 'Save File' },
    [',W'] = { '<cmd>noautocmd w!<cr>', desc = 'Save File(Without Aus)' },
    ['<A-h>'] = { '<cmd>tabp<cr>', desc = 'Switch to Prev Tab' },
    ['<A-l>'] = { '<cmd>tabn<cr>', desc = 'Switch to Next Tab' },
    ['<A-j>'] = { 'mz:m+<cr>`z', desc = 'Move line down' },
    ['<A-k>'] = { 'mz:m-2<cr>`z', desc = 'Move line up' },
    ['<C-p>'] = { '<cmd>Telescope find_files<cr>', desc = 'Find Files' },
    ['<C-n>'] = { '<cmd>Neotree focus toggle<cr>' },
    ['<A-/>'] = { '"ayy"apk<Plug>(comment_toggle_linewise_current)j', desc = 'Copy Line and Comment' },
    ['dl'] = { '"_dl' },
    ['c'] = { '"_c' },
    ['C'] = { '"_C' },
    ['Q'] = { '<cmd>copen<cr>', desc = 'Open Quickfix list' },
    ['k'] = { "v:count == 0 ? 'gk' : 'k'", expr = true, silent = true },
    ['j'] = { "v:count == 0 ? 'gj' : 'j'", expr = true, silent = true },
    [']q'] = { '<cmd>cn<cr>', desc = 'Quickfix Next ' },
    ['[q'] = { '<cmd>cprev<cr>', desc = 'Quickfix Prev ' },
    ['<leader>v'] = { 'v$', desc = 'Select till end' },
    ['<leader>1'] = { '1gt', desc = 'Goto tab 1' },
    ['<leader>2'] = { '2gt', desc = 'Goto tab 2' },
    ['<leader>3'] = { '3gt', desc = 'Goto tab 3' },
    ['<leader>4'] = { '4gt', desc = 'Goto tab 4' },
    ['<leader>5'] = { '5gt', desc = 'Goto tab 5' },
    ['<leader>6'] = { '6gt', desc = 'Goto tab 6' },
    ['<leader>7'] = { '7gt', desc = 'Goto tab 7' },
    ['<leader>8'] = { '8gt', desc = 'Goto tab 8' },
    ['<leader>9'] = { '9gt', desc = 'Goto tab 9' },
    ['<leader>b'] = {
      n = { ':bnext<cr>', 'Goto Buffer Next' },
      p = { ':bprev<cr>', 'Goto Buffer Prev' },
    },
    ['<leader>E'] = {
      name = 'Edit',
      e = { ':edit <C-r>=expand("%:p:h")<cr>/', 'here', silent = false },
      v = { ':vnew <C-r>=expand("%:p:h")<cr>/', 'in vsplit', silent = false },
      t = { ':tabedit <C-r>=expand("%:p:h")<cr>/', 'in new tab', silent = false },
    },
    ['<leader>C'] = { '<cmd>cd %:p:h<cr>:pwd<cr>', desc = 'Change directory to file' },
    ['<leader>fs'] = { '<cmd>Telescope live_grep<cr>', desc = '[F]ind [S]earch Project' },
    ['<leader>fN'] = {
      function()
        local builtin = require 'telescope.builtin'
        builtin.find_files({ cwd = vim.fn.stdpath 'config' })
      end,
      desc = 'Open Astro Configs',
    },
    ['<leader>fn'] = {
      function()
        local builtin = require 'telescope.builtin'
        local my_config = vim.fn.getenv 'XDG_CONFIG_HOME'
        if not my_config then
          my_config = vim.fn.getenv 'HOME' .. '/.config'
        end
        local cwd = my_config .. '/astronvim/lua'
        builtin.find_files({ cwd = cwd })
      end,
      desc = 'Open Astro User Configs',
    },
    ['<leader>fd'] = {
      function()
        local builtin = require 'telescope.builtin'
        local cwd = vim.fn.getenv 'HOME' .. '/Documents/dotfiles'
        builtin.find_files({ cwd = cwd, hidden = true })
      end,
      desc = 'Open Dotfiles',
    },
  },
  v = {
    p = {
      [[ 'pgv"'.v:register.'y' ]],
      expr = true,
      noremap = true,
      silent = false,
      desc = "Don't Copy when pasting in visual mode",
    },
    c = { '"_c' },
    J = { ":m '>+1<CR>gv=gv", desc = 'Move Line Down' },
    K = { ":m '<-2<CR>gv=gv", desc = 'Move Line Up' },
    ['<'] = { '<gv' },
    ['>'] = { '>gv' },
  },
  i = {
    [','] = { ',<C-g>u' },
    ['.'] = { '.<C-g>u' },
    ['?'] = { '?<C-g>u' },
    ['<C-\\>'] = { '<Esc><Cmd>exe v:count1 . "ToggleTerm"<CR>', desc = 'Open terminal' },
  },
  o = {
    ['ie'] = { ':exec "normal! ggVG"<cr>', desc = 'New operator for entire file' },
    ['iv'] = { ':exec "normal! HVL"<cr>', desc = 'New operator for entire file' },
  },
  t = {
    ['<C-l>'] = false,
    ['<C-k>'] = false,
  },
}
return config
