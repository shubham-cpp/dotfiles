require("which-key").setup({
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
		border = "single",
		position = "bottom",
		margin = { 1, 0, 1, 0 },
		padding = { 2, 2, 2, 2 },
	},
	hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ", "require" }, -- hide mapping boilerplate
	triggers_blacklist = {
		i = { "j", "k" },
		v = { "j", "k" },
	},
})

local mappings = {
	["1"] = { "1gt", "Goto tab 1" },
	["2"] = { "2gt", "Goto tab 2" },
	["3"] = { "3gt", "Goto tab 3" },
	["4"] = { "4gt", "Goto tab 4" },
	["5"] = { "5gt", "Goto tab 5" },
	["6"] = { "6gt", "Goto tab 6" },
	["7"] = { "7gt", "Goto tab 7" },
	["8"] = { "8gt", "Goto tab 8" },
	["9"] = { "9gt", "Goto tab 9" },
	-- single
	["+"] = { "<cmd>vertical resize +4<CR>", "Increase win size" },
	["-"] = { "<cmd>vertical resize -4<CR>", "Decrease win size" },
	["="] = "Format Code",
	["/"] = { '<CMD>lua require("Comment.api").call("toggle_current_linewise_op")<CR>g@$', "Toggle Comment" },
	["v"] = { "v$h", "Select till end" },

	c = {
		name = "Change",
		d = { "<cmd>cd %:p:h<cr>:pwd<cr>", "Directory" },
	},

	e = {
		name = "Edit",
		e = { ':edit <C-r>=expand("%:p:h")<cr>/', "here" },
		v = { ':vnew <C-r>=expand("%:p:h")<cr>/', "in vsplit" },
		t = { ':tabedit <C-r>=expand("%:p:h")<cr>/', "in new tab" },
	},

	-- g = {
	--   name = "Git",
	--   a = { '<cmd>!git add %:p<CR>',                                   'add current' },
	--   A = { '<cmd>!git add .<CR>',                                     'add all' },
	--   d = { '<cmd>DiffviewFileHistory<CR>',                            'diff file' },
	--   g = { 'lazygit' },
	--   h = {
	--     name = "Hunk",
	--     b = "blame line",
	--     p = "preview",
	--     r = "reset",
	--     s = "stage",
	--     u = "undo stage",
	--   },
	--   l = {
	--     name = "Log",
	--     a = "commits",
	--     c = "buffer commits",
	--   },
	--   m = { 'blame line' },
	--   s = { '<cmd>Telescope git_status<CR>',                           'status' },
	-- },

	f = {
		name = "Search",
		c = "color schemes",
		f = "Files",
		n = "Neovim config",
		d = "Dotfiles",
		s = "Current file",
		S = "Project",
		h = "Vim Help",
		H = "Recent Files",
		z = "Spelling",
	},

	-- n = {
	-- 	name = "Notes",
	-- 	a = "Show Tags",
	-- 	c = "Show Calendar",
	-- 	C = { "CalendarT", "Show Calendar(plugin)" },
	-- 	d = "Find Daily",
	-- 	f = "Find",
	-- 	i = "Insert Link",
	-- 	I = "Insert Link(Img)",
	-- 	n = "Create New",
	-- 	N = "Create Template",
	-- 	p = "Show Panel",
	-- 	r = "Rename",
	-- 	s = "Search",
	-- 	t = "Toggle Todo",
	-- 	T = { ":GenTocMarked<cr>", "Generate TOC" },
	-- 	y = "Yank Note Link",
	-- 	z = "Follow Link",
	-- },
	t = {
		name = "Toggle",
		m = { "toggle" },
		t = { "tableize" },
		g = "Lazygit",
		s = { "<cmd>setlocal spell!<cr>", "Toggle Spelling" },
	},
}

local wk = require("which-key")
wk.register(mappings, { prefix = "<leader>" })
wk.register({
	s = { [[ :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]], "Replace word" },
	w = { ":w!<cr>", "save file" },
	c = "Neovim config",
	d = "Dotfiles",
	z = "Spell suggest",
}, { prefix = "," })

wk.register({
	["]z"] = "TreeSitter swap next",
	["[z"] = "TreeSitter swap prev",
	["]]"] = "Next class start",
	["]["] = "Next class end",
	["[["] = "Prev class start",
	["[]"] = "Prev class end",
})

wk.register({
	s = "Switch",
	e = "Line Diagnostics",
	t = "Goto type definition",
}, { prefix = "g" })
