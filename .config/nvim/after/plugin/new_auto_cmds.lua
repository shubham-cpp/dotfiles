local map = function(options)
	options.opts = { buffer = true, noremap = true }
	options.mode = options.mode or "n"
	vim.keymap.set(options.mode, options.lhs, options.rhs, options.opts)
end
--- File Specific Keybindings {{{
local ft_key_binds = vim.api.nvim_create_augroup("FT_Keys", { clear = true })

local ft_key_binds_commands = {
	{
		event = { "FileType" },
		opts = {
			pattern = { "markdown" },
			callback = function()
				map({ lhs = "<leader>mp", rhs = ":MarkdownPreview<cr>" })
				map({ lhs = "<leader>mtt", rhs = ":GenTocMarked<cr>" })
				map({ lhs = "<leader>mtg", rhs = ":GenTocGitLab<cr>" })
			end,
			desc = "Keybindings for Markdown files",
		},
	},
}

for _, aucmd in ipairs(ft_key_binds_commands) do
	aucmd.opts.pattern = aucmd.opts.pattern or "*"
	aucmd.opts.group = ft_key_binds
	vim.api.nvim_create_autocmd(aucmd.event, aucmd.opts)
end
--- }}}
