-- Default packer config {{{
function get_config(name)
	return string.format('require("config/%s")', name)
end
local packer = require("packer")
packer.init({
	enable = true,
	threshold = 0,
})
local use = packer.use
packer.reset()
-- }}}
-- Packer can manage itself
use("wbthomason/packer.nvim")
use({ "lewis6991/impatient.nvim", rocks = "mpack" })
use("nathom/filetype.nvim")
-- Completions {{{
use({
	"hrsh7th/nvim-cmp",
	event = "InsertEnter",
	config = get_config("cmp"),
})
use({ "hrsh7th/cmp-nvim-lua", after = "nvim-cmp" })
use({ "hrsh7th/cmp-nvim-lsp", after = "cmp-nvim-lua" })
use({ "hrsh7th/cmp-buffer", after = "cmp-nvim-lua" })
use({ "hrsh7th/vim-vsnip", after = "cmp-buffer" })
use({ "hrsh7th/cmp-vsnip", after = "vim-vsnip" })
use({ "rafamadriz/friendly-snippets", after = "cmp-vsnip" })
use({ "hrsh7th/cmp-path", after = "friendly-snippets" })
use({ "hrsh7th/cmp-nvim-lsp-signature-help", after = "cmp-path" })
-- }}}
-- Lsp{{{
use({ "williamboman/nvim-lsp-installer", after = "cmp-nvim-lsp" })
use({ "b0o/schemastore.nvim", after = "nvim-lsp-installer" })
use({ "neovim/nvim-lspconfig", config = get_config("lsp"), after = "schemastore.nvim" })
use({
	"jose-elias-alvarez/null-ls.nvim",
	requires = { "nvim-lua/plenary.nvim" },
	event = "InsertEnter",
	config = get_config("lsp.nulls"),
})
-- }}}
-- Fuzzy Finder {{{
use({
	"nvim-telescope/telescope.nvim",
	config = get_config("telescope"),
	requires = { { "nvim-lua/plenary.nvim" } },
	keys = {
		"<leader>ff",
		"<leader>fn",
		"<leader>fd",
		"<leader>fc",
		"<leader>fh",
		"<leader>fH",
		"<leader>fs",
		"<leader>fS",
		"<leader>fz",
	},
})
use({ "natecraddock/telescope-zf-native.nvim" })
use({ "ibhagwan/fzf-lua", event = "BufWinEnter", config = get_config("fzf-lua") })
-- }}}
-- TreeSitter {{{
use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate", config = get_config("tree_sit"), event = "BufWinEnter" })
use({ "jose-elias-alvarez/nvim-lsp-ts-utils", after = "nvim-treesitter" })
use({ "JoosepAlviste/nvim-ts-context-commentstring", after = "nvim-lsp-ts-utils" })
use({ "windwp/nvim-ts-autotag", after = "nvim-ts-context-commentstring" })
use({ "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-ts-autotag" })
use({ "nvim-treesitter/playground", after = "nvim-treesitter-textobjects" })
-- }}}
-- Theming {{{
use({
	"navarasu/onedark.nvim",
	event = "VimEnter",
	disable = false,
	config = function()
		-- Options: deep | dark | darker | warm | warmer
		require("onedark").setup({
			style = "darker",
			transparent = true,
			highlights = {
				QuickScopePrimary = { fg = "$orange", fmt = "underline,italic" },
				QuickScopeSecondary = { fg = "$cyan", fmt = "underline,italic" },
			},
			code_style = {
				comments = "italic",
				keywords = "italic",
				functions = "none",
				strings = "none",
				variables = "none",
			},
		})
		require("onedark").load()
	end,
})

use({
	"marko-cerovac/material.nvim",
	event = "VimEnter",
	disable = true,
	config = function()
		-- Options: deep ocean | darker | palenight | oceanic
		vim.g.material_style = "deep ocean"
		local colors = require("material.colors")

		require("material").setup({
			custom_highlights = {
				QuickScopePrimary = { fg = colors.orange, style = "underline,italic" },
				QuickScopeSecondary = { fg = colors.cyan, style = "underline,italic" },
				TSConstructor = { fg = colors.darkpurple, style = "bold" },
			},
			italics = {
				comments = true,
				keywords = true,
				-- functions = "none",
				-- strings = "none",
				-- variables = "none",
			},
		})
		vim.cmd("colorscheme material")
	end,
})

use({
	"nvim-lualine/lualine.nvim",
	config = get_config("lualine"),
	event = "VimEnter",
	requires = { "kyazdani42/nvim-web-devicons", opt = true },
})
-- }}}
-- Motion {{{
use({
	"tpope/vim-surround",
	event = "BufReadPost",
})

use({
	"unblevable/quick-scope",
	-- event = "BufReadPre",
	keys = { "f", "F", "t", "T" },
	setup = function()
		vim.g.qs_highlight_on_keys = { "f", "F", "t", "T" }
		vim.g.qs_buftype_blacklist = {
			"terminal",
			"nofile",
			"fzf",
			"floaterm",
			"NvimTree",
		}
		vim.g.qs_lazy_highlight = 1
		vim.cmd([[ highlight! QuickScopePrimary guifg=orange gui=underline ctermfg=155 cterm=underline ]])
		vim.cmd([[ highlight! QuickScopeSecondary guifg=yellow gui=underline ctermfg=81 cterm=underline ]])
	end,
})

use({
	"phaazon/hop.nvim",
	config = function()
		require("hop").setup()
	end,
	cmd = { "HopChar2", "HopWord" },
})
-- }}}
-- Misc {{{
use({
	"norcalli/nvim-colorizer.lua",
	event = "BufReadPost",
	config = get_config("colors"),
})
use({ "tweekmonster/startuptime.vim", cmd = "StartupTime" })
use({ "svermeulen/vim-subversive", after = "vim-surround", config = get_config("subversive") })
use({ "tpope/vim-repeat", event = "InsertEnter" })
use({ "AndrewRadev/switch.vim", cmd = { "Switch", "SwitchReverse" } })
-- }}}
-- Git {{{
use({
	"lewis6991/gitsigns.nvim",
	requires = { "nvim-lua/plenary.nvim" },
	event = "BufReadPre",
	config = get_config("gitsigns"),
})

use({
	"TimUntersberger/neogit",
	cmd = "Neogit",
	config = function()
		require("neogit").setup({
			integrations = {
				diffview = true,
			},
		})
	end,
})

use({
	"sindrets/diffview.nvim",
	after = "neogit",
	config = function()
		require("diffview").setup({})
	end,
})
-- }}}
-- Enhance experience {{{
use({
	"lukas-reineke/indent-blankline.nvim",
	after = "gitsigns.nvim",
	-- event = "BufReadPre",
	config = get_config("indents"),
})
use({
	"akinsho/nvim-toggleterm.lua",
	keys = { "<F1>", "<Space>tg", "<C-\\>" },
	-- after = "Comment.nvim",
	config = get_config("toggle-term"),
})

use({ "numToStr/Comment.nvim", config = get_config("comments"), after = "nvim-treesitter-textobjects" })
use({ "windwp/nvim-autopairs", after = { "nvim-treesitter", "nvim-cmp" }, config = get_config("pairs") })
-- }}}
-- Ide bloat {{{
use({ "onsails/lspkind-nvim", event = "BufWinEnter" })
use({
	"kyazdani42/nvim-tree.lua",
	cmd = { "NvimTreeToggle", "NvimTreeFindFileToggle" },
	config = get_config("nvim-tree"),
})
use({
	"Shatur/neovim-session-manager",
	cmd = { "SaveSession", "LoadLastSession", "LoadCurrentDirSession" },
	config = get_config("sessions"),
})
use({ "folke/which-key.nvim", after = "indent-blankline.nvim", config = get_config("which-key") })
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
-- }}}
-- Additional FileTypes {{{
-- use("kovetskiy/sxhkd-vim")
-- }}}
