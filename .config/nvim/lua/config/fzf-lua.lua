local fzf = require('fzf-lua')
local actions = require('fzf-lua.actions')
local map = require('helper').map
local bmap = require('helper').bmap

local dotfiles = io.popen("readlink -f ~/.config/awesome/ | cut -d'.' -f1")
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
map('n', '??', '<cmd>FzfLua grep_visual<cr>')
map('n', '\\\\', '<cmd>FzfLua grep_curbuf<CR>')

local m_keys = {
	['alt-enter'] = actions.file_tabedit,
	['ctrl-x'] = actions.file_split,
}
fzf.setup({
	fzf_opts = { ['--info'] = 'hidden' },
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
