local fzf = require 'fzf-lua'
local actions = require 'fzf-lua.actions'
local map = require('sp.helper').map
local bmap = require('sp.helper').bmap
local opts = {
  winopts = {
    preview = {
      layout = 'vertical',
      vertical = 'up:40%',
      -- horizontal = 'right:60%',
    },
  },
}
local dotfiles = io.popen "readlink -f ~/.config/awesome/ | cut -d'.' -f1"
map('n', '<C-p>', '<cmd>FzfLua files<cr>')
map('n', ',c', '<cmd>FzfLua files cwd=~/.config/nvim<cr>')
if dotfiles then
  map('n', ',d', '<cmd>FzfLua files cwd=' .. dotfiles:read() .. '<cr>')
  dotfiles:close()
else
  map('n', ',d', '<cmd>FzfLua files cwd=~/Documents/dotfiles<cr>')
end
map('n', ',z', '<cmd>FzfLua spell_suggest<cr>')
map('n', ',g', '<cmd>FzfLua git_status<cr>')
map('n', '??', function()
  fzf.grep_project(opts)
end)
map('n', '\\\\', function()
  fzf.grep_curbuf(opts)
end)
map('n', '<leader>gla', '<cmd>FzfLua git_commits<cr>')
map('n', '<leader>glc', '<cmd>FzfLua git_bcommits<cr>')
map('n', '<leader>gs', '<cmd>FzfLua git_status<CR>')
map('n', '<leader>gb', '<cmd>FzfLua git_branches<CR>')

local m_keys = {
  ['alt-enter'] = actions.file_tabedit,
  ['ctrl-x'] = actions.file_split,
}
fzf.setup({
  fzf_opts = { ['--info'] = 'hidden' },
  winopts = {
    preview = { default = 'bat_native' },
  },
  on_create = function()
    bmap('t', '<C-j>', '<Down>')
    bmap('t', '<C-k>', '<Up>')
  end,
  previewers = {
    bat = {
      cmd = 'bat',
      args = '--style=changes',
    },
  },
  icons = {
    ['?'] = { icon = '?', color = 'magenta' },
    ['M'] = { icon = '★', color = 'red' },
    ['D'] = { icon = '✗', color = 'red' },
    ['A'] = { icon = '+', color = 'green' },
  },
  files = {
    winopts = {
      height = 0.55,
      width = 0.65,
      row = 0.52,
      col = 0.47,
    },
    previewer = { _ctor = false },
    actions = m_keys,
  },
  git = {
    status = {
      actions = m_keys,
      prompt = ' ❯ ',
    },
    bcommits = { actions = m_keys },
  },
  buffers = { actions = m_keys },
  blines = { actions = m_keys },
})
-- map('n', 'gr', function()
-- 	fzf.lsp_references({
-- 		winopts = {
-- 			-- split = 'aboveleft new',
-- 			-- height = 0.65,
-- 			-- width = 0.7,
-- 			-- row = 0.9,
-- 			-- col = 200,
-- 			preview = {
-- 				layout = 'vertical',
-- 				vertical = 'up:40%',
-- 				-- horizontal = 'right:60%',
-- 			},
-- 		},
-- 	})
-- end)
