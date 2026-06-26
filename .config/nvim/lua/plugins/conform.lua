local u = require("config.utils")

local M = {}

vim.pack.add({
	u.gh("stevearc/conform.nvim"),
})

local conform = require("conform")

local prettier = { "prettierd", "prettier", stop_after_first = true }
local prettier_condition = function()
	return not (vim.b.disable_prettier or vim.g.disabled_prettier)
end
---@param bufnr integer
---@param ... string
---@return string
local function first(bufnr, ...)
	for i = 1, select("#", ...) do
		local formatter = select(i, ...)
		if conform.get_formatter_info(formatter, bufnr).available then
			return formatter
		end
	end
	return select(1, ...)
end

-- local function prettier_eslint(bufnr)
--   return { first(bufnr, "prettierd", "prettier"), "eslint_d" }
--   -- return { first(bufnr, "prettierd", "prettier") }
-- end

function M.setup()
	conform.setup({
		formatters_by_ft = {
			lua = { "stylua" },
			javascript = prettier,
			typescript = prettier,
			javascriptreact = prettier,
			typescriptreact = prettier,
			html = prettier,
			css = prettier,
			scss = prettier,
			less = prettier,
			json = prettier,
			jsonc = prettier,
			yaml = prettier,
			astro = prettier,
			handlebars = prettier,
			markdown = prettier,
			["markdown.mdx"] = prettier,
			vue = prettier,
			svelte = prettier,
			toml = { "taplo" },
			bash = { "shfmt" },
			sh = { "shfmt" },
			fish = { "fish_indent" },
			go = function(bufnr)
				return { "goimports", first(bufnr, "gofumpt", "gofmt") }
			end,
			python = { "ruff_format", "ruff_fix", "ruff_organize_imports" },
		},
		default_format_opts = {
			lsp_format = "fallback",
			timeout_ms = 1500,
		},
		format_on_save = function(bufnr)
			if not u.should_apply(bufnr, true) then
				return nil
			end

			return {
				timeout_ms = 1000,
				lsp_format = "fallback",
			}
		end,
		formatters = {
			prettierd = { condition = prettier_condition },
			prettier = { condition = prettier_condition },
		},
	})
	vim.keymap.set({ "n", "x" }, "<leader>=", M.format, { desc = "Format" })
end

M.format = function()
	if not u.should_apply(0, true) then
		return
	end

	conform.format({ async = true, lsp_format = "fallback" })
end

M.setup()

return M
