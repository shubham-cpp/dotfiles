local null_ls = require("null-ls")
local b = null_ls.builtins
local has_eslint_config = function(u)
	return u.root_has_file(".eslintrc")
		or u.root_has_file(".eslintrc.json")
		or u.root_has_file(".eslintrc.js")
		or u.root_has_file(".eslintrc.cjs")
		or u.root_has_file(".eslintrc.yaml")
		or u.root_has_file(".eslintrc.yml")
end
local add_svelte = {
	"javascript",
	"javascriptreact",
	"typescript",
	"typescriptreact",
	"vue",
	"svelte",
}
local sources = {
	-- Html,css,json
	b.formatting.prettierd,
	-- [java|type]script(react)?
	-- b.formatting.fixjson,
	b.diagnostics.eslint_d.with({
		condition = has_eslint_config,
		filetypes = add_svelte,
	}),
	b.formatting.eslint_d.with({
		condition = has_eslint_config,
	}),
	b.code_actions.eslint_d.with({
		condition = has_eslint_config,
		filetypes = add_svelte,
	}),
	-- C/C++
	b.formatting.clang_format,
	b.diagnostics.cppcheck,
	-- Lua
	b.diagnostics.luacheck.with({
		extra_args = { "--globals", "vim", "describe" },
	}),
	b.formatting.stylua,
	-- Python
	b.formatting.isort,
	b.formatting.yapf,
	b.diagnostics.flake8,
	-- Bash,zsh
	b.formatting.shfmt.with({ extra_args = { "-i", "2", "-ci" } }),
	b.diagnostics.shellcheck,
	-- Vim
	b.diagnostics.vint,
	-- Yaml
	b.diagnostics.yamllint,
	-- Misc
	b.code_actions.gitsigns,
	-- b.hover.dictionary,
}
null_ls.setup({
	on_attach = function(_, bufnr)
		vim.api.nvim_buf_set_keymap(
			bufnr,
			"n",
			"<leader>=",
			"<cmd>lua vim.lsp.buf.formatting()<cr>",
			{ noremap = true }
		)
		print("LSP attached (null-ls)")
	end,
	sources = sources,
	diagnostics_format = "[#{c}] #{m} (#{s})",
	debug = true,
})
