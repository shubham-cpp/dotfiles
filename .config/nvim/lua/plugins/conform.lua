local prettier = { "prettierd", "prettier" }
local slow_format_filetypes = {}
return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	keys = {
		{
			-- Customize or remove this keymap to your liking
			"<leader>=",
			function()
				require("conform").format({ async = true, lsp_fallback = true })
			end,
			mode = "",
			desc = "Format buffer(c)",
		},
	},
	opts = {
		-- Define your formatters
		formatters_by_ft = {
			sh = { "shfmt" },
			lua = { "stylua" },
			bash = { "shfmt" },
			c = { "clang-format" },
			cpp = { "clang-format" },
			go = { "goimports", { "gofumpt", "gofmt" } },
			fish = { "fish_indent" },
			css = { prettier },
			html = { prettier },
			jsonc = { prettier },
			json = { prettier },
			json5 = { prettier },
			less = { prettier },
			markdown = { prettier },
			sass = { prettier },
			scss = { prettier },
			astro = { prettier, "eslint_d" },
			javascript = { prettier, "eslint_d" },
			javascriptreact = { prettier, "eslint_d" },
			typescript = { prettier, "eslint_d" },
			typescriptreact = { prettier, "eslint_d" },
			svelte = { prettier, "eslint_d" },
			vue = { prettier, "eslint_d" },
			zig = { "zigfmt" },
			-- You can use a function here to determine the formatters dynamically
			python = function(bufnr)
				if require("conform").get_formatter_info("ruff_format", bufnr).available then
					return { "ruff_format" }
				else
					return { "isort", "black" }
				end
			end,
			-- Use the "_" filetype to run formatters on filetypes that don't
			-- have other formatters configured.
			["_"] = { "trim_whitespace", "trim_newlines" },
		},
		-- Set up format-on-save
		-- format_on_save = { timeout_ms = 500, lsp_fallback = true },
		format_on_save = function(bufnr)
			-- Disable autoformat for files in a certain path
			local bufname = vim.api.nvim_buf_get_name(bufnr)
			if bufname:match("/node_modules/") then
				return
			end
			if slow_format_filetypes[vim.bo[bufnr].filetype] then
				return
			end
			local function on_format(err)
				if err and err:match("timeout$") then
					slow_format_filetypes[vim.bo[bufnr].filetype] = true
				end
			end

			return { timeout_ms = 200, lsp_fallback = true }, on_format
		end,

		format_after_save = function(bufnr)
			if not slow_format_filetypes[vim.bo[bufnr].filetype] then
				return
			end
			return { lsp_fallback = true }
		end,
		-- Customize formatters
		formatters = {
			shfmt = {
				prepend_args = { "-i", "2" },
			},
		},
	},
	init = function()
		-- If you want the formatexpr, here is the place to set it
		vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
	end,
}