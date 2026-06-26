local u = require("config.utils")

local M = {}

vim.pack.add({
	u.gh("mfussenegger/nvim-lint"),
})

local lint = require("lint")

local function resolve_linter(name)
	local linter = lint.linters[name]
	if type(linter) == "function" then
		linter = linter()
	end
	return linter
end

local function command_for(name)
	local linter = resolve_linter(name)
	if type(linter) ~= "table" then
		return nil
	end

	local cmd = linter.cmd
	if type(cmd) == "function" then
		cmd = cmd()
	end

	return cmd
end

local function installed_linters(bufnr)
	local names = lint.linters_by_ft[vim.bo[bufnr].filetype] or {}
	local available = {}

	for _, name in ipairs(names) do
		local cmd = command_for(name)
		if type(cmd) == "string" and vim.fn.executable(cmd) == 1 then
			table.insert(available, name)
		end
	end

	return available
end

function M.setup()
	lint.linters_by_ft = {
    go = { "golangcilint" },
    json = { "jq" },
    python = { "ruff" },
    fish = { "fish" },
	}

	vim.api.nvim_create_autocmd("BufWritePost", {
		group = vim.api.nvim_create_augroup("ConfigNvimLint", { clear = true }),
		desc = "Run linters after save",
		callback = function(args)
			if not u.should_apply(args.buf) then
				return
			end

			local names = installed_linters(args.buf)
			if #names == 0 then
				return
			end

			lint.try_lint(names)
		end,
	})
end

M.setup()


return M
