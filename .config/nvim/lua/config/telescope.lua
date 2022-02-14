local M = {}
local map = require("helper").map
local telescope = require("telescope")
local actions = require("telescope.actions")
local action_layout = require("telescope.actions.layout")

local center_list = require("telescope.themes").get_dropdown({
	winblend = 10,
	layout_config = { width = 0.5, height = 0.8 },
	previewer = false,
})

function M.fd_nvim()
	local opts = require("telescope.themes").get_dropdown({})
	opts.previewer = false
	opts.prompt_prefix = "Nvim>"
	opts.cwd = vim.fn.stdpath("config")
	require("telescope.builtin").fd(opts)
end

function M.fd_dotfiles()
	local opts = require("telescope.themes").get_dropdown({})
	opts.previewer = false
	opts.hidden = true
	opts.cwd = os.getenv("HOME") .. "/Documents/dotfiles"
	require("telescope.builtin").find_files(opts)
end

function M.find_files()
	local dropdown_theme = require("telescope.themes").get_dropdown({
		borderchars = {
			{ "─", "│", "─", "│", "┌", "┐", "┘", "└" },
			prompt = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
			results = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
			preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
		},
		width = 0.8,
		previewer = false,
		prompt_title = false,
	})
	require("telescope.builtin").fd(dropdown_theme)
end

function M.grep_current()
	local opts = vim.deepcopy(center_list)
	opts.prompt_prefix = "Goto 🔍"
	require("telescope.builtin").current_buffer_fuzzy_find(opts)
end

telescope.setup({
	defaults = {
		vimgrep_arguments = {
			"rg",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case",
			"--trim", -- add this value
		},
		mappings = {
			i = {
				["<esc>"] = actions.close,
				["<A-CR>"] = actions.select_tab,
				["<M-p>"] = action_layout.toggle_preview,
			},
			n = { ["<A-CR>"] = actions.select_tab, ["<M-p>"] = action_layout.toggle_preview },
		},
		prompt_prefix = "🔍",
		initial_mode = "insert",
		file_ignore_patterns = {
			".backup",
			".swap",
			".langservers",
			".undo",
			".git",
			"node_modules",
			"vendor",
			".cache",
			".vscode%",
			"classes",
			".venv",
			"%.png",
			"%.jpeg",
			"%.jpg",
			"%.mkv",
			"%.mp3",
			"%.mp4",
			"%.out",
			"%.class",
			"__pycache__",
			"%.o",
			"patches",
			"packer_compiled.lua",
		},
	},
})
telescope.load_extension("zf-native")
vim.cmd([[ command! -bang Nvim :lua require'config.telescope'.fd_nvim()<CR> ]])
vim.cmd([[ command! -bang DotFiles :lua require'config.telescope'.fd_dotfiles()<CR> ]])
vim.cmd([[ command! -bang MGrep :lua require'config.telescope'.grep_current()<CR> ]])

map("n", "<leader>ff", M.find_files)
map("n", "<leader>fn", M.fd_nvim)
map("n", "<leader>fd", M.fd_dotfiles)
map("n", "<leader>fs", M.grep_current)
map("n", "<leader>fh", require("telescope.builtin").help_tags)
map("n", "<leader>fH", require("telescope.builtin").oldfiles)
map("n", "<leader>fc", require("telescope.builtin").colorscheme)
map("n", "<leader>fz", require("telescope.builtin").spell_suggest)

return M
