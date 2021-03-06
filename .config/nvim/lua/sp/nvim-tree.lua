local tree_cb = require('nvim-tree.config').nvim_tree_callback

local list = {
	{ key = { 'e', 'l' }, cb = tree_cb('edit') },
	{ key = '<M-CR>', cb = tree_cb('tabnew') },
	{ key = 'h', cb = tree_cb('code_node') },
	{ key = '.', cb = tree_cb('toggle_dotfiles') },
	{ key = '?', cb = tree_cb('toggle_help') },
	{ key = 'd', cb = tree_cb('trash') },
	{ key = 'D', cb = tree_cb('remove') },
}
require('nvim-tree').setup({
	-- auto_close = true,
	auto_reload_on_write = true,
	diagnostics = {
		enable = true,
		show_on_dirs = true,
	},
	renderer = { add_trailing = true },
	update_cwd = true,
	filters = {
		dotfiles = true,
		custom = {
			'.git',
			'node_modules',
			'.cache',
			'__pycache__',
			'.vscode',
			'.steam',
			'.mozilla',
			'.pki',
		},
	},
	view = { mappings = { list = list }, relativenumber = true, width = '35%' },
	trash = {
		cmd = 'trash',
		require_confirm = false,
	},
	system_open = {
		cmd = 'xdg-open',
		args = {},
	},
})
