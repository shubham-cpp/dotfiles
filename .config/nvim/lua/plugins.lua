-- Default packer config {{{
function get_config(name)
	return string.format('require("config/%s")', name)
end
local packer = require('packer')
packer.init({
	enable = true,
	threshold = 0,
})
local use = packer.use
packer.reset()
-- }}}
-- Packer can manage itself
use('wbthomason/packer.nvim')
use('lewis6991/impatient.nvim')
use('nathom/filetype.nvim')
-- Completions {{{
use({
	'hrsh7th/nvim-cmp',
	config = get_config('cmp'),
	-- event = 'BufEnter',
	requires = {
		{ 'hrsh7th/cmp-nvim-lua' },
		{ 'hrsh7th/cmp-nvim-lsp' },
		{ 'hrsh7th/cmp-buffer' },
		{ 'hrsh7th/vim-vsnip' },
		{ 'hrsh7th/cmp-vsnip' },
		{ 'rafamadriz/friendly-snippets' },
		{ 'hrsh7th/cmp-path' },
		{ 'hrsh7th/cmp-nvim-lsp-signature-help' },
		{ 'hrsh7th/cmp-cmdline' },
		{ 'f3fora/cmp-spell' },
		-- {
		-- 	'hrsh7th/cmp-copilot',
		-- 	requires = { 'github/copilot.vim' },
		-- },
		{ 'tzachar/cmp-tabnine', run = './install.sh' },
	},
})
-- }}}
-- Lsp{{{
use({
	'neovim/nvim-lspconfig',
	config = get_config('lsp'),
	after = 'nvim-cmp',
	requires = {
		{ 'b0o/schemastore.nvim' },
		{ 'williamboman/nvim-lsp-installer' },
		{
			'j-hui/fidget.nvim',
			config = function()
				require('fidget').setup({
					text = {
						spinner = 'dots_pulse',
					},
					align = {
						bottom = true,
					},
					window = {
						relative = 'editor',
					},
				})
			end,
			after = 'nvim-lspconfig',
		},
	},
})
use({
	'jose-elias-alvarez/null-ls.nvim',
	requires = { 'nvim-lua/plenary.nvim' },
	config = get_config('lsp.nulls'),
})
-- }}}
-- Fuzzy Finder {{{
use({ 'nvim-lua/plenary.nvim' })
use({
	'nvim-telescope/telescope.nvim',
	config = get_config('telescope'),
	keys = {
		'<leader>ff',
		'<leader>fn',
		'<leader>fd',
		'<leader>fc',
		'<leader>fh',
		'<leader>fH',
		'<leader>fs',
		'<leader>fS',
		'<leader>fz',
	},
	requires = { { 'natecraddock/telescope-zf-native.nvim' } },
})
use({ 'ibhagwan/fzf-lua', config = get_config('fzf-lua') })
-- }}}
-- TreeSitter {{{
use({ 'nvim-treesitter/nvim-treesitter', after = 'nvim-lspconfig', run = ':TSUpdate', config = get_config('tree_sit') })
use({ 'jose-elias-alvarez/nvim-lsp-ts-utils', after = 'nvim-treesitter' })
use({ 'JoosepAlviste/nvim-ts-context-commentstring', after = 'nvim-lsp-ts-utils' })
use({ 'windwp/nvim-ts-autotag', after = 'nvim-ts-context-commentstring' })
use({ 'nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-ts-autotag' })
use({ 'nvim-treesitter/playground', cmd = 'TSPlaygroundToggle' })
use({ 'andymass/vim-matchup', after = 'nvim-treesitter-textobjects' })
-- }}}
-- Theming {{{
use({
	'navarasu/onedark.nvim',
	-- event = 'VimEnter',
	disable = true,
	config = function()
		-- Options: deep | dark | darker | warm | warmer
		require('onedark').setup({
			style = 'darker',
			transparent = true,
			highlights = {
				QuickScopePrimary = { fg = '$orange', fmt = 'underline,italic' },
				QuickScopeSecondary = { fg = '$cyan', fmt = 'underline,italic' },
			},
			code_style = {
				comments = 'italic',
				keywords = 'italic',
				functions = 'none',
				strings = 'none',
				variables = 'none',
			},
		})
		require('onedark').load()
	end,
})

use({
	'catppuccin/nvim',
	as = 'catppuccin',
	-- event = 'VimEnter',
	disable = false,
	config = function()
		require('catppuccin').setup({
			-- transparent_background = true,
			styles = {
				comments = 'italic',
				functions = 'italic',
				keywords = 'italic',
				strings = 'NONE',
				variables = 'NONE',
			},
			integrations = {
				nvimtree = {
					enabled = true,
					show_root = true,
					transparent_panel = true,
				},
				which_key = true,
				indent_blankline = {
					enabled = true,
					colored_indent_levels = true,
				},
				neogit = true,
				hop = true,
			},
		})
		local colors = require('catppuccin.api.colors').get_colors()
		local cat = require('catppuccin')
		cat.remap({ QuickScopePrimary = { fg = colors.red } })
		cat.remap({ QuickScopeSecondary = { fg = colors.green } })
		vim.g.catppuccin_flavour = 'mocha' -- latte, frappe, macchiato, mocha
		vim.cmd('colorscheme catppuccin')
	end,
})

use({
	'sainnhe/gruvbox-material',
	-- event = 'VimEnter',
	disable = true,
	setup = function()
		vim.g.gruvbox_material_background = 'hard'
		vim.g.gruvbox_material_better_performance = 1
		vim.g.gruvbox_material_enable_bold = 1
		vim.g.gruvbox_material_enable_italic = 1
		vim.g.gruvbox_material_diagnostic_text_highlight = 1
		vim.g.gruvbox_material_diagnostic_line_highlight = 1
		vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'

		vim.g.gruvbox_material_palette = 'material' -- 'material'`, `'mix'`, `'original'
	end,
	config = function()
		vim.cmd('colorscheme gruvbox-material')
	end,
})

use({
	'marko-cerovac/material.nvim',
	-- event = 'VimEnter',
	disable = true,
	config = function()
		-- Options: deep ocean | darker | palenight | oceanic
		vim.g.material_style = 'deep ocean'
		vim.g.material_disable_background = true
		local colors = require('material.colors')

		require('material').setup({
			custom_highlights = {
				QuickScopePrimary = { fg = colors.orange, style = 'underline,italic' },
				QuickScopeSecondary = { fg = colors.cyan, style = 'underline,italic' },
				TSConstructor = { fg = colors.darkpurple, style = 'bold' },
			},
			contrast = {
				floating_windows = true,
				popup_menu = true,
				sign_column = true,
			},
			italics = {
				comments = true,
				keywords = true,
				-- functions = "none",
				-- strings = "none",
				-- variables = "none",
			},
			disable = {
				background = true,
			},
			lualine_style = 'stealth',
		})
		vim.cmd('colorscheme material')
	end,
})

use({
	'nvim-lualine/lualine.nvim',
	-- event = 'VimEnter',
	config = get_config('lualine'),
	requires = { 'kyazdani42/nvim-web-devicons', opt = true },
})
-- }}}
-- Motion {{{
use({
	'tpope/vim-surround',
	-- event = 'BufWinEnter',
})

use({
	'unblevable/quick-scope',
	keys = { 'f', 'F', 't', 'T' },
	setup = function()
		vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
		vim.g.qs_buftype_blacklist = {
			'terminal',
			'nofile',
			'fzf',
			'floaterm',
			'NvimTree',
		}
		vim.g.qs_lazy_highlight = 1
		vim.cmd([[ highlight! QuickScopePrimary guifg=orange gui=underline ctermfg=155 cterm=underline ]])
		vim.cmd([[ highlight! QuickScopeSecondary guifg=yellow gui=underline ctermfg=81 cterm=underline ]])
	end,
})

use({
	'phaazon/hop.nvim',
	config = function()
		require('hop').setup()
	end,
	cmd = { 'HopChar2', 'HopWord' },
})
-- }}}
-- Misc {{{
use({
	'norcalli/nvim-colorizer.lua',
	ft = { 'lua', 'vim', 'css', 'html', 'javascriptreact', 'typescriptreact', 'scss', 'less', 'json' },
	config = get_config('colors'),
})
use({ 'tweekmonster/startuptime.vim', cmd = 'StartupTime' })
use({ 'svermeulen/vim-subversive', after = 'vim-surround', config = get_config('subversive') })
use({ 'tpope/vim-repeat', after = 'vim-subversive' })
use({ 'AndrewRadev/switch.vim', cmd = { 'Switch', 'SwitchReverse' } })
-- }}}
-- Git {{{
use({
	'lewis6991/gitsigns.nvim',
	-- event = 'BufWinEnter',
	requires = { 'nvim-lua/plenary.nvim' },
	config = get_config('gitsigns'),
})
use({
	'TimUntersberger/neogit',
	cmd = { 'Neogit' },
	config = function()
		require('neogit').setup({
			integrations = {
				diffview = true,
			},
		})
	end,
	requires = {
		{
			'sindrets/diffview.nvim',
			after = 'neogit',
			config = function()
				require('diffview').setup({})
			end,
		},
	},
})
use({
	'akinsho/git-conflict.nvim',
	-- event = 'BufWinEnter',
	config = function()
		require('git-conflict').setup()
	end,
})
-- }}}
-- Enhance experience {{{
use({
	'lukas-reineke/indent-blankline.nvim',
	after = 'gitsigns.nvim',
	config = get_config('indents'),
})
use({
	'akinsho/nvim-toggleterm.lua',
	keys = { '<F1>', '<C-\\>', '<leader>tg' },
	config = get_config('toggle-term'),
})

use({ 'numToStr/Comment.nvim', config = get_config('comments'), after = 'nvim-treesitter-textobjects' })
use({ 'windwp/nvim-autopairs', after = { 'nvim-treesitter', 'nvim-cmp' }, config = get_config('pairs') })
use({
	'iamcco/markdown-preview.nvim',
	run = 'cd app && npm install',
	ft = { 'markdown' },
	setup = function()
		vim.g.mkdp_refresh_slow = 1
	end,
	requires = {
		{
			'mzlogin/vim-markdown-toc',
			after = 'markdown-preview.nvim',
		},
	},
})
use({
	'danymat/neogen',
	config = function()
		require('neogen').setup({})
	end,
	cmd = { 'Neogen' },
})
use({
	'mg979/vim-visual-multi',
	setup = function()
		vim.g.VM_maps = {}
		vim.g.VM_mouse_mappings = 1
		vim.g.VM_maps = {
			['Find Under'] = '<M-d>',
			['Find Subword Under'] = '<M-d>',
			['Skip Region'] = '<C-x>',
			['Select All'] = '<M-a>',
			['Start Regex Search'] = '\\/',
		}
	end,
})
-- }}}
-- Ide bloat {{{
use({ 'onsails/lspkind-nvim' })
use({
	'kyazdani42/nvim-tree.lua',
	cmd = { 'NvimTreeToggle', 'NvimTreeFindFileToggle' },
	config = get_config('nvim-tree'),
})
use({
	'stevearc/dressing.nvim',
	after = 'lualine.nvim',
	config = function()
		require('dressing').setup({
			select = {
				backend = { 'fzf_lua', 'telescope', 'builtin' },
			},
		})
	end,
})
use({
	'Shatur/neovim-session-manager',
	after = 'dressing.nvim',
	requires = { 'nvim-lua/plenary.nvim' },
	config = function()
		require('session_manager').setup({
			-- Define what to do when Neovim is started without arguments.
			-- Possible values: Disabled, CurrentDir, LastSession
			autoload_mode = require('session_manager.config').AutoloadMode.CurrentDir,
		})
	end,
})
use({ 'folke/which-key.nvim', after = 'indent-blankline.nvim', config = get_config('which-key') })
use({
	'rcarriga/vim-ultest',
	requires = { 'vim-test/vim-test' },
	ft = { 'javascript', 'javascriptreact', 'typescriptreact', 'typescript' },
	run = ':UpdateRemotePlugins',
	config = function()
		vim.cmd('let test#javascript#reactscripts#options = "--watchAll=false"')
		local map = function(lhs, rhs)
			vim.keymap.set('n', lhs, rhs, { noremap = true })
		end
		map(']t', '<Plug>(ultest-next-fail)')
		map('[t', '<Plug>(ultest-prev-fail)')
		map('<leader>ts', '<cmd>UltestSummary<cr>')
		map('<leader>tn', '<cmd>UltestNearest<cr>')
		map('<leader>tt', '<cmd>Ultest<cr>')
	end,
})
-- }}}
-- Note Taking {{{
-- use({
-- 	"renerocksai/telekasten.nvim",
-- 	requires = {
-- 		{ "iamcco/markdown-preview.nvim", ft = { "md", "markdown" } },
-- 		{
-- 			"renerocksai/calendar-vim",
-- 			cmd = { "Calendar", "CalendarH", "CalendarT" },
-- 		},
-- 		{ "mzlogin/vim-markdown-toc", cmd = { "GenTocGFM", "GenTocGitLab", "GenTocMarked" } },
-- 	},
-- 	config = get_config("telekasten"),
-- 	keys = { "<leader>nn", "<leader>nf", "<leader>nd", "<leader>ns" },
-- })
-- use({
-- 	'nvim-neorg/neorg',
-- 	config = get_config('neorg'),
-- })
-- use({
-- 	"nvim-orgmode/orgmode",
-- 	after = "nvim-treesitter-textobjects",
-- 	config = get_config("org"),
-- })
-- }}}
