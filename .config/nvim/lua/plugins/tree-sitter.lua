return {
	"nvim-treesitter/nvim-treesitter",
	event = "BufRead",
	version = false, -- last release is way too old and doesn't work on Windows
	build = ":TSUpdate",
	dependencies = {
		"nvim-treesitter/nvim-treesitter-textobjects",
		-- { "nvim-treesitter/playground", cmd = "TSPlaygroundToggle" },
		"andymass/vim-matchup",
		"windwp/nvim-ts-autotag",
		{
			"nvim-treesitter/nvim-treesitter-context",
			opts = {},
		},
		-- HACK: remove when https://github.com/windwp/nvim-ts-autotag/issues/125 closed.
	},

	config = function()
		require("nvim-treesitter.configs").setup({
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = { "markdown", "xml" },
				disable = function(_, bufnr)
					local line_count = vim.api.nvim_buf_line_count(bufnr)
					if line_count > 2500 then
						return true
					end
				end,
			},
			matchup = { enable = true },
			autotag = {
				enable = true,
				filetypes = {
					"astro",
					"html",
					"javascript",
					"javascriptreact",
					"markdown",
					"svelte",
					"typescriptreact",
					"vue",
					"xml",
				},
				--[[ enable_close_on_slash = true 
          enable_rename = true,
          enable_close = true,
          --]]
			},
			ensure_installed = {
				"bash",
				"c",
				"comment",
				"cpp",
				"css",
				"dockerfile",
				"fish",
				"go",
				"gomod",
				"gosum",
				"gowork",
				"html",
				"javascript",
				"jsdoc",
				"json",
				"json5",
				"jsonc",
				"lua",
				"luadoc",
				"markdown",
				"markdown_inline",
				"python",
				"query",
				"regex",
				"rasi",
				"rust",
				"scss",
				"sxhkdrc",
				"svelte",
				"tsx",
				"todotxt",
				"typescript",
				"vim",
				"vimdoc",
				"vue",
				"yaml",
				"xml",
			},
			textobjects = {
				select = {
					enable = true,
					lookahead = true,
					keymaps = {
						["af"] = "@function.outer",
						["if"] = "@function.inner",
						["ac"] = "@class.outer",
						["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
						["al"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
					},
					selection_modes = {
						["@parameter.outer"] = "v", -- charwise
						["@function.outer"] = "V", -- linewise
						["@class.outer"] = "<c-v>", -- blockwise
					},
					include_surrounding_whitespace = true,
				},
				move = {
					enable = true,
					set_jumps = true, -- whether to set jumps in the jumplist
					goto_next_start = {
						["]m"] = "@function.outer",
						["]]"] = { query = "@class.outer", desc = "Next class start" },
						["]l"] = "@loop.*",
						-- ["]o"] = { query = { "@loop.inner", "@loop.outer" } }
						["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
						["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
					},
					goto_next_end = {
						["]M"] = "@function.outer",
						["]["] = "@class.outer",
					},
					goto_previous_start = {
						["[m"] = "@function.outer",
						["[["] = "@class.outer",
					},
					goto_previous_end = {
						["[M"] = "@function.outer",
						["[]"] = "@class.outer",
					},
					goto_next = {
						["]i"] = "@conditional.outer",
					},
					goto_previous = {
						["[i"] = "@conditional.outer",
					},
				},
				swap = {
					enable = true,
					swap_next = {
						[">K"] = { query = "@block.outer", desc = "Swap next block" },
						[">F"] = { query = "@function.outer", desc = "Swap next function" },
						[">A"] = { query = "@parameter.inner", desc = "Swap next argument" },
					},
					swap_previous = {
						["<K"] = { query = "@block.outer", desc = "Swap previous block" },
						["<F"] = { query = "@function.outer", desc = "Swap previous function" },
						["<A"] = { query = "@parameter.inner", desc = "Swap previous argument" },
					},
				},
			},
		})
		vim.keymap.set("n", "<LocalLeader>c", function()
			require("treesitter-context").go_to_context(vim.v.count1)
		end, { silent = true, desc = "Goto Context" })
		vim.keymap.set("n", "<leader><up>", function()
			require("treesitter-context").go_to_context(vim.v.count1)
		end, { silent = true, desc = "Goto Context" })
	end,
}
