return require('packer').startup(function(use)
	use('wbthomason/packer.nvim')
	use('lewis6991/impatient.nvim')
	use('nathom/filetype.nvim')

	-- Nvim cmp {{{
	use({
		'hrsh7th/nvim-cmp',
		event = 'InsertEnter',
	})
	use({ 'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp' })
	use({ 'hrsh7th/cmp-nvim-lsp', after = 'cmp-nvim-lua' })
	use({ 'hrsh7th/cmp-nvim-lsp-signature-help', after = 'cmp-nvim-lsp' })
	use({ 'hrsh7th/cmp-buffer', after = 'cmp-nvim-lsp-signature-help' })
	use({ 'hrsh7th/vim-vsnip', after = 'cmp-buffer' })
	use({ 'hrsh7th/cmp-vsnip', after = 'vim-vsnip' })
	use({ 'rafamadriz/friendly-snippets', after = 'cmp-vsnip' })
	use({ 'hrsh7th/cmp-path', after = 'friendly-snippets' })
	use({ 'hrsh7th/cmp-cmdline', after = 'cmp-path' })
	use({ 'tzachar/cmp-tabnine', run = './install.sh', after = 'cmp-cmdline' })
	use({
		'f3fora/cmp-spell',
		after = 'cmp-tabnine',
		config = function()
			require('sp.cmp')
		end,
	})
	-- }}}

	-- LSP {{{
	use({
		'jose-elias-alvarez/null-ls.nvim',
		requires = { 'nvim-lua/plenary.nvim' },
		after = 'cmp-spell',
		config = function()
			require('sp.nulls')
		end,
	})
	use({ 'williamboman/nvim-lsp-installer', after = 'null-ls.nvim' })
	use({ 'b0o/schemastore.nvim', after = 'nvim-lsp-installer' })
	use({
		'neovim/nvim-lspconfig',
		after = 'schemastore.nvim',
		config = function()
			require('sp.lsp')
		end,
	})

	use({
		'j-hui/fidget.nvim',
		config = function()
			require('sp.fidget')
		end,
		after = 'nvim-lspconfig',
	})
	-- }}}

	-- Fuzzy Finders {{{

	use({
		'nvim-telescope/telescope.nvim',
		config = function()
			require('sp.telescope')
		end,
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
		requires = { { 'natecraddock/telescope-zf-native.nvim' }, { 'nvim-lua/plenary.nvim' } },
	})
	use({
		'ibhagwan/fzf-lua',
		config = function()
			require('sp.fzf-lua')
		end,
	})
	-- }}}

	-- TreeSitter {{{
	use({
		'nvim-treesitter/nvim-treesitter',
		event = 'BufWinEnter',
		run = ':TSUpdate',
		config = function()
			require('sp.tree')
		end,
	})
	use({ 'jose-elias-alvarez/nvim-lsp-ts-utils', after = 'nvim-treesitter' })
	use({ 'JoosepAlviste/nvim-ts-context-commentstring', after = 'nvim-lsp-ts-utils' })
	use({ 'windwp/nvim-ts-autotag', after = 'nvim-ts-context-commentstring' })
	use({ 'nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-ts-autotag' })
	use({ 'nvim-treesitter/playground', cmd = 'TSPlaygroundToggle' })
	-- use({ 'andymass/vim-matchup', after = 'nvim-treesitter-textobjects' })
	-- }}}

	-- Theming {{{
	use({
		'ellisonleao/gruvbox.nvim',
		disable = false,
		event = 'VimEnter',
		config = function()
			require('sp.colors.gruvbox')
		end,
	})
	use({
		'navarasu/onedark.nvim',
		event = 'VimEnter',
		disable = true,
		config = function()
			require('sp.colors.onedark')
		end,
	})
	use({
		'catppuccin/nvim',
		as = 'catppuccin',
		event = 'VimEnter',
		disable = true,
		config = function()
			require('sp.colors.cat')
		end,
	})
	use({
		'marko-cerovac/material.nvim',
		event = 'VimEnter',
		disable = true,
		config = function()
			require('sp.colors.material')
		end,
	})
	use({
		'nvim-lualine/lualine.nvim',
		event = 'VimEnter',
		config = function()
			require('sp.lualine')
		end,
		requires = { 'kyazdani42/nvim-web-devicons', opt = true },
	})
	-- }}}

	-- Motion {{{
	use({ 'tpope/vim-surround', after = 'git-conflict.nvim' })
	use({
		'wellle/targets.vim',
		after = 'vim-repeat',
		config = function()
			vim.cmd([[
            augroup MyTargets
            au!
autocmd User targets#mappings#user call targets#mappings#extend({
    \ 'b': {'pair': [{'o':'(', 'c':')'}, {'o':'[', 'c':']'}, {'o':'{', 'c':'}'},{'o':'<', 'c':'>'}]},
    \ 's': { 'separator': [{'d':','}, {'d':'.'}, {'d':';'}, {'d':':'}, {'d':'+'}, {'d':'-'},
    \                      {'d':'='}, {'d':'~'}, {'d':'_'}, {'d':'*'}, {'d':'#'}, {'d':'/'},
    \                      {'d':'\'}, {'d':'|'}, {'d':'&'}, {'d':'$'}] },
    \ })
    augroup END
]])
		end,
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
			-- vim.cmd([[ highlight! QuickScopePrimary guifg=orange gui=underline ctermfg=155 cterm=underline ]])
			-- vim.cmd([[ highlight! QuickScopeSecondary guifg=yellow gui=underline ctermfg=81 cterm=underline ]])
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
		config = function()
			require('sp.colorizer')
		end,
	})
	use({ 'tweekmonster/startuptime.vim', cmd = 'StartupTime' })
	use({
		'svermeulen/vim-subversive',
		after = 'vim-surround',
		config = function()
			require('sp.subversive')
		end,
	})
	use({ 'tpope/vim-repeat', after = 'vim-subversive' })
	use({ 'AndrewRadev/switch.vim', cmd = { 'Switch', 'SwitchReverse' } })

	-- }}}

	-- Git {{{
	use({
		'lewis6991/gitsigns.nvim',
		event = 'BufReadPost',
		-- after = 'neovim-session-manager',
		requires = { 'nvim-lua/plenary.nvim' },
		config = function()
			require('gitsigns').setup()
		end,
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
		after = 'gitsigns.nvim',
		-- event = 'BufWinEnter',
		config = function()
			require('git-conflict').setup()
		end,
	})
	-- }}}

	-- Enhance experience {{{
	use({
		'lukas-reineke/indent-blankline.nvim',
		-- after = 'git-conflict.nvim',
		after = 'gruvbox.nvim',
		-- event = 'VimEnter',
		config = function()
			require('sp.indents')
		end,
	})
	use({
		'akinsho/nvim-toggleterm.lua',
		keys = { '<F1>', '<C-\\>', '<leader>tg', '<leader>tf' },
		config = function()
			require('sp.toggle-term')
		end,
	})

	use({
		'numToStr/Comment.nvim',
		config = function()
			require('sp.comments')
		end,
		after = 'nvim-treesitter-textobjects',
	})
	use({
		'windwp/nvim-autopairs',
		after = { 'nvim-treesitter', 'cmp-spell' },
		config = function()
			require('sp.pairs')
		end,
	})
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
		after = 'pretty-fold.nvim',
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
	use({
		'anuvyklack/pretty-fold.nvim',
		after = 'which-key.nvim',
		config = function()
			require('sp.folds')
		end,
	})
	-- }}}

	-- IDE bloat {{{
	use({
		'kyazdani42/nvim-tree.lua',
		cmd = { 'NvimTreeToggle', 'NvimTreeFindFileToggle' },
		config = function()
			require('sp.nvim-tree')
		end,
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
	use({
		'folke/which-key.nvim',
		after = 'indent-blankline.nvim',
		config = function()
			require('sp.which-key')
		end,
	})
	-- }}}

	-- Additional FTs {{{
	use('baskerville/vim-sxhkdrc')
	-- }}}
end)
