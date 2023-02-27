local o = vim.opt
local g = vim.g
local cmd = vim.cmd
local cache = os.getenv 'XDG_CACHE_HOME' or os.getenv 'HOME' .. '/.cache'

g.loaded_matchit = 1

cmd [[set runtimepath=$VIMRUNTIME]]
-- cmd [[set packpath=/tmp/nvim/site]]
o.packpath = cache .. '/nvim-vscode/site'
o.packpath:append '~/.config/nvim-vscode'

local package_root = cache .. '/nvim-vscode/site/pack'
local install_path = package_root .. '/packer/start/packer.nvim'

-- Plugins
local function load_plugins()
	require('packer').startup({
		{
			'wbthomason/packer.nvim',
			'tpope/vim-repeat',
			'sainnhe/everforest',
			'tpope/vim-commentary',
			'unblevable/quick-scope',
			'andymass/vim-matchup',
			{
				'tommcdo/vim-lion',
				setup = function()
					vim.g.lion_squeeze_spaces = 1
				end,
			},
			{
				'svermeulen/vim-subversive',
				config = function()
					vim.keymap.set('n', 'x', '<Plug>(SubversiveSubstitute)', { silent = true, noremap = false })
					vim.keymap.set('n', 'xx', '<Plug>(SubversiveSubstituteLine)', { silent = true, noremap = false })
					vim.keymap.set('n', 'X', '<Plug>(SubversiveSubstituteToEndOfLine)', { silent = true, noremap = false })
				end,
			},
			{
				'junegunn/vim-easy-align',
				config = function()
					vim.keymap.set({ 'n', 'x' }, 'ga', '<Plug>(EasyAlign)', { silent = true, noremap = false })
				end,
			},
			{
				'kylechui/nvim-surround',
				config = function()
					require('nvim-surround').setup({})
				end,
			},
			-- {
			-- 	'Exafunction/codeium.vim',
			-- 	config = function()
			-- 		vim.g.codeium_disable_bindings = 1
			-- 		-- Change '<C-g>' here to any keycode you like.
			-- 		vim.keymap.set('i', '<C-g>', function()
			-- 			return vim.fn['codeium#Accept']()
			-- 		end, { expr = true })
			-- 		vim.keymap.set('i', '<c-;>', function()
			-- 			return vim.fn['codeium#CycleCompletions'](1)
			-- 		end, { expr = true })
			-- 		vim.keymap.set('i', '<c-,>', function()
			-- 			return vim.fn['codeium#CycleCompletions'](-1)
			-- 		end, { expr = true })
			-- 		vim.keymap.set('i', '<c-x>', function()
			-- 			return vim.fn['codeium#Clear']()
			-- 		end, { expr = true })
			-- 	end,
			-- },
			{
				'ggandor/leap.nvim',
				config = function()
					require('leap').add_default_mappings()
				end,
			},
			{
				'monaqa/dial.nvim',
				setup = function()
					local map = function(mode, lhs, rhs)
						vim.keymap.set(mode, lhs, rhs, { noremap = false, silent = false })
					end
					map({ 'n', 'x' }, '<C-a>', '<Plug>(dial-increment)')
					map({ 'n', 'x' }, '<C-x>', '<Plug>(dial-decrement)')
					map('x', 'g<C-a>', 'g<Plug>(dial-increment)')
					map('x', 'g<C-x>', 'g<Plug>(dial-decrement)')
				end,
				config = function()
					local augend = require 'dial.augend'
					local webs = {
						augend.integer.alias.decimal,
						augend.integer.alias.hex,
						augend.constant.alias.bool,
						augend.constant.new({
							elements = { '&&', '||' },
							word = false,
							cyclic = true,
						}),
						augend.constant.new({ elements = { 'let', 'const' } }),
					}
					local py_lua = {
						augend.integer.alias.decimal,
						augend.integer.alias.hex,
						augend.constant.alias.bool,
						augend.constant.new({
							elements = { 'and', 'or' },
							word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
							cyclic = true, -- "or" is incremented into "and".
						}),
					}
					require('dial.config').augends:register_group({
						default = {
							augend.integer.alias.decimal,
							augend.integer.alias.hex,
							augend.constant.alias.bool,
							augend.constant.new({
								elements = { '&&', '||' },
								word = false,
								cyclic = true,
							}),
						},
						javascript = {
							augend.integer.alias.decimal,
							augend.integer.alias.hex,
							augend.constant.alias.bool,
							augend.constant.new({
								elements = { '&&', '||' },
								word = false,
								cyclic = true,
							}),
							augend.constant.new({ elements = { 'let', 'const' } }),
						},
						lua = py_lua,
						python = {
							augend.integer.alias.decimal,
							augend.integer.alias.hex,
							augend.constant.new({
								elements = { 'and', 'or' },
								word = false, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
								cyclic = true, -- "or" is incremented into "and".
							}),
							augend.constant.new({
								elements = { 'True', 'False' },
								word = false,
								cyclic = true,
							}),
						},
						visual = {
							augend.integer.alias.decimal,
							augend.integer.alias.hex,
							augend.date.alias['%Y/%m/%d'],
							augend.constant.alias.alpha,
							augend.constant.alias.Alpha,
						},
					})
				end,
			},
			{
				'Darazaki/indent-o-matic',
				config = function()
					local iom = require 'indent-o-matic'
					iom.setup({})
				end,
			},
		},
		config = {
			package_root = package_root,
			compile_path = '~/.config/nvim-vscode/plugin/packer_compiled.lua',
		},
	})
end

-- Options
g.everforest_background = 'hard'
g.everforest_better_performance = 1
-- g.qs_highlight_on_keys = {'f', 'F', 't', 'T'}

vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
g.qs_lazy_highlight = 1
g.mapleader = ' '
-- g.maplocalleader = '\\'
o.number = true
o.relativenumber = true
o.clipboard:append({ 'unnamedplus' })
o.mouse = 'a'
o.path:append({ '**' })
o.splitbelow = true
o.splitright = true
o.fileignorecase = true
-- o.backupdir = vim.fn.stdpath 'cache' .. '/backups//'
-- o.backup = true
-- o.backupcopy = 'yes'
-- o.writebackup = true
-- o.swapfile = true
-- o.directory = vim.fn.stdpath 'cache' .. '/swaps//'
-- o.history = 500
-- o.cmdheight = 1
o.updatetime = 300
-- o.shortmess:append 'c'
-- o.signcolumn = 'yes'
o.iskeyword:append '-'
-- o.wildignorecase = true
-- o.wildmode = 'list:lastused'
-- o.wildignore:prepend({
--   '*.out,*.o,*.pyc,*~,*.class,*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store,*/venv/*,*/__pycache__/*,*.jpg,*.png,*.svg,*.jpeg,*.jpg',
--   '*.mp4',
--   '**/node_modules/*',
--   '**/build/*',
--   '**/coverage/*',
--   '*.mkv',
-- })
o.ignorecase = true
o.smartcase = true
-- o.tagcase = 'smart'
-- o.lazyredraw = true
-- o.showmatch = true
o.whichwrap:append '<,>,[,]'
-- o.timeoutlen = 500
-- o.errorbells = false
-- o.visualbell = false
o.scrolloff = 8
o.expandtab = true
o.shiftwidth = 4
o.tabstop = 4
-- o.cursorline = true
-- o.wrap = true
-- o.breakindent = true
-- o.linebreak = true
-- o.showbreak = '>> '
-- o.numberwidth = 6
-- o.list = false
-- o.smartindent = true
-- o.showtabline = 2
-- o.showmode = false
if vim.fn.has 'termguicolors' == 1 then
	o.termguicolors = true
end
-- o.tags:append({ './.git/tags' })
-- o.inccommand = 'nosplit'
-- o.undofile = true
-- o.laststatus = 3
if vim.fn.executable 'rg' == 1 then
	o.grepprg = 'rg --vimgrep --smart-case --hidden --follow'
end

cmd.colorscheme 'everforest'

cmd [[ command! Q :q! ]]
cmd [[ cabbrev vf vert sf ]]

local disabled_built_ins = {
	'netrw',
	'netrwPlugin',
	'netrwSettings',
	'netrwFileHandlers',
	'gzip',
	'zip',
	'zipPlugin',
	'tar',
	'tarPlugin',
	'getscript',
	'getscriptPlugin',
	'vimball',
	'vimballPlugin',
	'2html_plugin',
	'logipat',
	'rrhelper',
	'spellfile_plugin',
	'matchit',
}

for _, plugin in pairs(disabled_built_ins) do
	vim.g['loaded_' .. plugin] = 1
end

if vim.fn.isdirectory(install_path) == 0 then
	vim.fn.system({ 'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path })
	load_plugins()
	require('packer').sync()
else
	load_plugins()
	require('packer').sync()
end

local map = function(mode, lhs, rhs, options)
	local opts = (options == nil or next(options) == nil) and { silent = true, noremap = true } or options
	vim.keymap.set(mode, lhs, rhs, opts)
end

map('', '0', '^', { noremap = false, silent = true })
map('n', '<Esc>', ':nohl<cr>')
map('n', '*', '*zz')
map('n', '#', '#zz')
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')

map('n', '<M-j>', 'mz:m+<cr>`z')
map('n', '<M-k>', 'mz:m-2<cr>`z')
map('n', '<S-Left>', ':tabp<cr>')
map('n', '<S-Right>', ':tabn<cr>')
map('n', '<C-Left>', ':tabp<cr>')
map('n', '<C-Right>', ':tabn<cr>')

map('n', '<C-h>', '<C-W>h')
map('n', '<C-j>', '<C-W>j')
map('n', '<C-k>', '<C-W>k')
map('n', '<C-l>', '<C-W>l')

map('n', 'j', 'gj', { noremap = false })
map('n', 'k', 'gk', { noremap = false })

-- Pasting in visual mode doesn't copy
map('x', 'p', [[ 'pgv"'.v:register.'y' ]], { noremap = true, expr = true })
map('n', 'dl', '"_dl')
map('n', 'c', '"_c')
map('n', 'C', '"_C')
map('x', 'c', '"_c')

map('c', '<C-a>', '<Home>', { noremap = false, silent = false })
map('c', '<C-e>', '<End>', { noremap = false, silent = false })

vim.cmd [[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]]
map('x', ',s', '"sy:let @/=@s<CR>cgn')

map('x', 'J', ":m '>+1<CR>gv=gv")
map('x', 'K', ":m '<-2<CR>gv=gv")
map('x', '<', '<gv')
map('x', '>', '>gv')

map('i', ',', ',<C-g>u')
map('i', '.', '.<C-g>u')
map('i', '?', '?<C-g>u')

map('n', '<M-->', ':exe "vertical resize -10"<CR>')
map('n', '<M-=>', ':exe "vertical resize +10"<CR>')

-- map('x', '<leader>y', '"+y')
-- map('x', '<leader>p', '"+p')

-- Plugin mappings
-- map('n', '<F7>', ':ColorizerToggle<cr>')

map('o', 'ie', ':exec "normal! ggVG"<cr>', { noremap = true, silent = true })
map('o', 'iv', ':exec "normal! HVL"<cr>', { noremap = true, silent = true })

if vim.g.vscode then
	-- cmd [[ nmap j gj ]]
	-- cmd [[ nmap k gk ]]
	map('', 'gD', "<Cmd>call VSCodeNotify('editor.action.revealDefinitionAside')<CR>")
	map('', 'gr', "<Cmd>call VSCodeNotify('editor.action.goToReferences')<CR>")
	map('', '[s', "<Cmd>call VSCodeNotify('editor.action.toggleStickyScroll')<CR>")
	map('n', ',d', '<cmd>Neogen<cr>', { noremap = true })

	map(
		'x',
		'<leader>f',
		[[<Cmd>call VSCodeNotifyRangePos('editor.action.formatSelection', line("v"), line("."), col("v"), col("."), 1)<CR>]]
	)

	map('n', '<C-Up>', "<Cmd>call VSCodeNotify('editor.action.insertCursorAbove')<cr>", { noremap = false })
	map('n', '<C-Down>', "<Cmd>call VSCodeNotify('editor.action.insertCursorBelow')<cr>", { noremap = false })
	map('n', '<C-Right>', "<Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>")
	map('n', '<C-Left>', "<Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>")

	map('n', '<C-b>', "<Cmd>call VSCodeNotify('workbench.action.toggleSidebarVisibility')<cr>")
	map('n', '<C-j>', "<Cmd>call VSCodeNotify('workbench.action.terminal.toggleTerminal')<cr>")

	map('n', '<leader>o', "<Cmd>call VSCodeNotify('workbench.action.openRecent')<cr>")

	map('n', '<leader>gg', "<Cmd>call VSCodeNotify('workbench.view.scm')<cr>")
	map('n', '<leader>ge', "<Cmd>call VSCodeNotify('workbench.view.extensions')<cr>")
	map('n', '<leader>gs', "<Cmd>call VSCodeNotify('workbench.view.search.toggleVisibility')<cr>")
	map('n', '<leader>gf', "<Cmd>call VSCodeNotify('workbench.view.explorer')<cr>")
	map('n', '<leader>gk', "<Cmd>call VSCodeNotify('workbench.action.openGlobalKeybindings')<cr>")

	map('n', ',w', "<Cmd>call VSCodeNotify('workbench.action.files.save')<cr>")

	map('n', ',W', "<Cmd>call VSCodeNotify('workbench.action.files.saveWithoutFormatting')<cr>")
	map('n', 'gh', "<Cmd>call VSCodeNotify('editor.action.showHover')<cr>")
	-- VsCode Folding
	map('', 'za', "<Cmd>call VSCodeNotify('editor.toggleFold')<CR>")
	map('', 'zC', "<Cmd>call VSCodeNotify('editor.foldAll')<CR>")
	map('', 'zO', "<Cmd>call VSCodeNotify('editor.unfoldAll')<CR>")
	map('', 'zp', "<Cmd>call VSCodeNotify('editor.gotoParentFold')<CR>")
end

vim.api.nvim_set_hl(0, 'QuickScopePrimary', { fg = '#afff5f', bold = true, underline = true, ctermfg = 'lightgreen' })
vim.api.nvim_set_hl(0, 'QuickScopeSecondary', { fg = '#5fffff', bold = true, underline = true, ctermfg = 'lightblue' })

local highlight_yank = vim.api.nvim_create_augroup('Highlight_Yank', { clear = true })

local highlight_yank_commands = {
	{
		event = 'TextYankPost',
		opts = {

			callback = function()
				vim.highlight.on_yank()
			end,
			desc = 'Show highlight when copying,deleting',
		},
	},
	{
		event = 'FileType',
		opts = {
			callback = function()
				vim.opt_local.shiftwidth = 2
				vim.opt_local.tabstop = 2
				vim.opt_local.softtabstop = 2
				vim.keymap.set(
					{ 'n', 'v' },
					'<C-a>',
					'"=javascript<CR><Plug>(dial-increment)',
					{ noremap = false, buffer = true }
				)
				vim.keymap.set(
					{ 'n', 'v' },
					'<C-x>',
					'"=javascript<CR><Plug>(dial-decrement)',
					{ noremap = false, buffer = true }
				)
			end,
			pattern = {
				'css',
				'javascript',
				'javascriptreact',
				'typescript',
				'typescriptreact',
				'html',
				'json',
				'vue',
				'svelte',
			},
			desc = 'Clean whitespaces',
		},
	},
	{
		event = 'FileType',
		opts = {
			command = 'nnoremap <buffer> q :q<cr>',
			pattern = { 'help', 'man' },
			desc = 'Use q to close help/man window',
		},
	},
	{
		event = 'FileType',
		opts = {
			command = 'nmap <buffer> <C-a> "=python<CR><Plug>(dial-increment) | nmap <buffer> <C-x> "=python<CR><Plug>(dial-decrement)  ',
			pattern = 'python',
			desc = 'Using dial.nvim for python',
		},
	},
	{
		event = 'FileType',
		opts = {
			command = 'nmap <buffer> <C-a> "=lua<CR><Plug>(dial-increment) | nmap <buffer> <C-x> "=python<CR><Plug>(dial-decrement)  ',
			pattern = 'lua',
			desc = 'Using dial.nvim for python',
		},
	},
	{
		event = 'FileType',
		opts = {
			command = [[ setlocal commentstring=##\ %s ]],
			pattern = { 'apache', 'sxhkdrc', 'toml', 'fish', 'template', 'mbsyncrc' },
		},
	},
	{
		event = 'FileType',
		opts = {
			command = [[ setlocal commentstring=;;\ %s ]],
			pattern = { 'lisp' },
		},
	},
	{
		event = 'FileType',
		opts = {
			command = [[ setlocal commentstring=<!--%s--> ]],
			pattern = { 'htmldjango' },
		},
	},
	{
		event = 'FileType',
		opts = {
			command = [[ setlocal commentstring=!\ %s ]],
			pattern = { 'xdefaults' },
		},
	},
	{
		event = 'BufEnter',
		opts = {
			command = [[ setlocal ft=lisp | setlocal commentstring=;;\ %s ]],
			pattern = { '*.kbd' },
		},
	},
	{
		event = 'BufEnter',
		opts = {
			callback = function()
				vim.opt.commentstring = '" %s'
			end,
			pattern = { '*.vifm' },
		},
	},
	{
		event = 'FileType',
		opts = {
			-- command = 'set formatoptions-=c formatoptions-=r formatoptions-=o'
			-- this should not be necessary because
			callback = function()
				vim.opt_local.formatoptions = vim.opt_local.formatoptions
					- 'a' -- Auto formatting is BAD.
					- 't' -- Don't auto format my code. I got linters for that.
					- '2' -- I'm not in gradeschool anymore
					- 'l' -- Yes break the lines
					- 'o' -- O and o, don't continue comments
					+ 'c' -- In general, I like it when comments respect textwidth
					+ 'q' -- Allow formatting comments w/ gq
					+ 'r' -- But do continue when pressing enter.
					+ 'n' -- Indent past the formatlistpat, not underneath it.
					+ 'j' -- Auto-remove comments if possible.
			end,
			pattern = '*',
			desc = 'Pressing O/o inside comment wont open another comment',
		},
	},
	{
		event = 'FileType',
		opts = {
			command = 'setlocal comments-=:// comments+=f://',
			pattern = { 'c', 'cpp' },
		},
	},
	{
		event = 'FileType',
		opts = {
			callback = function()
				vim.opt_local.formatprg =
					'stylua --call-parentheses NoSingleString --indent-width 2 --quote-style AutoPreferSingle -'
			end,
			pattern = { 'lua' },
		},
	},
}

for _, cmds in ipairs(highlight_yank_commands) do
	if cmds.opts.callback == nil and cmds.opts.command == nil then
		print "You didn't specify and command or callback for autocommand"
		return 1
	end
	cmds.opts.pattern = cmds.opts.pattern or '*'
	cmds.opts.group = highlight_yank
	vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end
