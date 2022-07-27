local Terminal = require('toggleterm.terminal').Terminal
local toggleterm = require('toggleterm')
local map = require('helper').map
local bmap = require('helper').bmap

function _G.set_terminal_keymaps()
	local opts = { noremap = true }
	-- bmap("t", "<esc>", [[<C-\><C-n>]], opts)
	bmap('t', '<C-]>', [[<C-\><C-n>]], opts)
	bmap('t', '<C-h>', [[<C-\><C-n><C-W>h]], opts)
	bmap('t', '<C-j>', [[<C-\><C-n><C-W>j]], opts)
	bmap('t', '<C-k>', [[<C-\><C-n><C-W>k]], opts)
	bmap('t', '<C-l>', [[<C-\><C-n><C-W>l]], opts)
	bmap('n', 'A', 'A<C-k>', opts)
	bmap('n', 'D', 'A<C-k><C-\\><C-n>', opts)
	bmap('n', 'cc', 'A<C-e><C-u>', opts)
	bmap('n', 'cc', 'A<C-e><C-u>', opts)
	bmap('n', 'dd', 'A<C-e><C-u><C-><C-n>', opts)
end

local lazygit = Terminal:new({
	cmd = 'lazygit',
	dir = 'git_dir',
	direction = 'float',
	float_opts = {
		border = 'curved',
	},
})

function Lazygit_toggle()
	lazygit:toggle()
end

local ranger = Terminal:new({
	cmd = 'ranger',
	-- dir = 'git_dir',
	direction = 'float',
	float_opts = {
		border = 'curved',
	},
})

function Ranger_toggle()
	ranger:toggle()
end

toggleterm.setup({
	size = 20,
	-- open_mapping = [[<F1>]],
	open_mapping = [[<c-\>]],
	shell = 'fish',
	shade_filetypes = {},
	shade_terminals = true,
	start_in_insert = true,
	persist_size = true,
	close_on_exit = true,
	-- Options: 'vertical' | 'horizontal' | 'window' | 'float',
	direction = 'float',
	float_opts = {
		-- The border key is *almost* the same as 'nvim_win_open'
		-- see :h nvim_win_open for details on borders however
		-- the 'curved' border is a custom border type
		-- not natively supported but implemented in this plugin.
		border = 'curved', -- single/double/shadow/curved
		highlights = {
			border = 'Normal',
			background = 'Normal',
		},
	},
})
vim.cmd([[
    augroup ToggleTerm
        autocmd!
        au FileType toggleterm set nonu
        au TermOpen term://*toggleterm#* lua set_terminal_keymaps()
    augroup END
]])
vim.cmd([[ command! -count=1 Vifm  lua require'toggleterm'.exec("vifm",<count>, 12) ]])
map('n', '<Space>tg', function()
	lazygit:toggle()
end)
map('n', '<Space>tf', function()
	ranger:toggle()
end)
