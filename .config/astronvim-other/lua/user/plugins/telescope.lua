local function getVisualSelection()
	vim.cmd('noau normal! "vy"')
	local text = vim.fn.getreg("v")
	vim.fn.setreg("v", {})

	text = string.gsub(text, "\n", "")
	if #text > 0 then
		return text
	else
		return ""
	end
end

return {
	"nvim-telescope/telescope.nvim",
	keys = {
		{ "<C-p>",      "<cmd>Telescope find_files<cr>",    desc = "Find Files" },
		{ "<leader>fw", "<cmd>Telescope grep_string<cr>",   desc = "[G]rep current word" },
		{ "<leader>fr", "<cmd>Telescope resume<cr>",        desc = "[R]esume" },
		{ "<leader>fR", "<cmd>Telescope registers<cr>",     desc = "[R]egisters" },
		{ "<leader>fz", "<cmd>Telescope spell_suggest<cr>", desc = "[S]pellings" },
		{ "<leader>fs", "<cmd>Telescope live_grep<cr>",     desc = "Search Project" },
		{
			"<leader>fs",
			function()
				local text = getVisualSelection()
				require("telescope.builtin").live_grep({ default_text = text })
			end,
			mode = "v",
			desc = "[S]earch [S]election",
		},
		{
			"<leader>fS",
			"<cmd>Telescope current_buffer_fuzzy_find<cr>",
			desc = "Search(Current Buffer)",
		},
		{
			"<leader>fS",
			function()
				local text = getVisualSelection()
				require("telescope.builtin").current_buffer_fuzzy_find({ default_text = text })
			end,
			mode = "v",
			desc = "[S]earch [S]election(Buffer)",
		},
		{
			"<leader>fn",
			require("user.lsp.util").cmd("Telescope find_files cwd=~/.config/astronvim/lua/user"),
			desc = "Astronvim User Config",
		},
		{
			"<leader>fN",
			require("user.lsp.util").cmd("Telescope find_files cwd=~/.config/nvim"),
			desc = "Astronvim Config",
		},
		{
			"<leader>fd",
			require("user.lsp.util").cmd("Telescope find_files cwd=~/Documents/dotfiles/.config hidden=true"),
			desc = "Dotfiles",
		},
	},
	opts = function(_, opts)
		local actions = require("telescope.actions")
		local action_layout = require("telescope.actions.layout")
		local dropdown = {
			layout_strategy = "vertical",
			layout_config = { width = 0.6, preview_cutoff = 1, prompt_position = "top" },
		}
		-- opts.defaults.mappings.i['<A-CR>'] = actions.select_tab
		-- opts.defaults.mappings.n['<A-CR>'] = actions.select_tab
		opts.defaults.mappings.n["<A-p>"] = action_layout.toggle_preview
		opts.defaults.mappings.i["<A-p>"] = action_layout.toggle_preview
		opts.defaults.mappings.i["<C-c>"] = actions.close
		opts.pickers = {
			lsp_references = dropdown,
			lsp_definitions = dropdown,
			git_branches = dropdown,
			git_commits = dropdown,
			git_bcommits = dropdown,
			lsp_document_symbols = dropdown,
			lsp_workspace_symbols = dropdown,
		}
		return opts
	end,
}
