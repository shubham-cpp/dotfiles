local M = {}
local map = require('helper').map
local telescope = require('telescope')
local themes = require('telescope.themes')
local builtin = require('telescope.builtin')
local actions = require('telescope.actions')
local action_layout = require('telescope.actions.layout')

local center_list = themes.get_dropdown({
	winblend = 10,
	layout_config = { width = 0.5, height = 0.8 },
	previewer = false,
})

function M.fd_nvim()
	local opts = themes.get_dropdown({})
	opts.previewer = false
	opts.prompt_prefix = '  '
	opts.cwd = vim.fn.stdpath('config')
	builtin.fd(opts)
end

function M.fd_dotfiles()
	local opts = themes.get_dropdown({})
	opts.previewer = false
	opts.hidden = true
	opts.prompt_prefix = '  '
	opts.cwd = os.getenv('HOME') .. '/Documents/dotfiles'
	builtin.find_files(opts)
end

function M.find_files()
	local dropdown_theme = themes.get_dropdown({
		borderchars = {
			{ '─', '│', '─', '│', '┌', '┐', '┘', '└' },
			prompt = { '─', '│', ' ', '│', '┌', '┐', '│', '│' },
			results = { '─', '│', '─', '│', '├', '┤', '┘', '└' },
			preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
		},
		width = 0.8,
		previewer = false,
		prompt_title = false,
	})
	builtin.fd(dropdown_theme)
end

function M.grep_current()
	local opts = vim.deepcopy(center_list)
	opts.prompt_prefix = 'Goto 🔍'
	builtin.current_buffer_fuzzy_find(opts)
end

telescope.setup({
	defaults = {
		vimgrep_arguments = {
			'rg',
			'--color=never',
			'--no-heading',
			'--with-filename',
			'--line-number',
			'--column',
			'--smart-case',
			'--trim',
		},
		mappings = {
			i = {
				['<esc>'] = actions.close,
				['<A-CR>'] = actions.select_tab,
				['<M-p>'] = action_layout.toggle_preview,
			},
			n = { ['<A-CR>'] = actions.select_tab, ['<M-p>'] = action_layout.toggle_preview },
		},
		prompt_prefix = '🔍',
		initial_mode = 'insert',
		file_ignore_patterns = {
			'.backup',
			'.swap',
			'.langservers',
			'.undo',
			'.git',
			'node_modules',
			'vendor',
			'.cache',
			'.vscode%',
			'classes',
			'.venv',
			'%.png',
			'%.jpeg',
			'%.jpg',
			'%.mkv',
			'%.mp3',
			'%.mp4',
			'%.out',
			'%.class',
			'__pycache__',
			'%.o',
			'patches',
			'packer_compiled.lua',
		},
	},
})
telescope.load_extension('zf-native')
vim.cmd([[ command! -bang Nvim :lua require'config.telescope'.fd_nvim()<CR> ]])
vim.cmd([[ command! -bang DotFiles :lua require'config.telescope'.fd_dotfiles()<CR> ]])
vim.cmd([[ command! -bang MGrep :lua require'config.telescope'.grep_current()<CR> ]])

map('n', '<leader>ff', "<cmd>lua require('config.telescope').find_files()<cr>")
map('n', '<leader>fn', "<cmd>lua require('config.telescope').fd_nvim()<cr>")
map('n', '<leader>fd', "<cmd>lua require('config.telescope').fd_dotfiles()<cr>")
map('n', '<leader>fs', "<cmd>lua require('config.telescope').grep_current()<cr>")
map('n', '<leader>fS', "<cmd>lua require('telescope.builtin').live_grep()<cr>")
map('n', '<leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>")
map('n', '<leader>fH', "<cmd>lua require('telescope.builtin').oldfiles()<cr>")
map('n', '<leader>fc', "<cmd>lua require('telescope.builtin').colorscheme()<cr>")
map('n', '<leader>fz', "<cmd>lua require('telescope.builtin').spell_suggest()<cr>")
map('n', '<leader>fg', "<cmd>lua require('telescope.builtin').git_status()<cr>")
map('n', '<leader>fb', "<cmd>lua require('telescope.builtin').buffers()<cr>")

return M
