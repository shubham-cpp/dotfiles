local null_ls = require('null-ls')
local b = null_ls.builtins
local format = b.formatting
local lint = b.diagnostics
local code_actions = b.code_actions
local has_eslint_config = function(u)
	return u.root_has_file('.eslintrc')
		or u.root_has_file('.eslintrc.json')
		or u.root_has_file('.eslintrc.js')
		or u.root_has_file('.eslintrc.cjs')
		or u.root_has_file('.eslintrc.yaml')
		or u.root_has_file('.eslintrc.yml')
end
local add_svelte = {
	'javascript',
	'javascriptreact',
	'typescript',
	'typescriptreact',
	'vue',
	'svelte',
}
local sources = {
	-- Html,css,json
	-- format.prettierd.with({
	-- 	disabled_filetypes = { --[[ "json", ]]
	-- 		'markdown',
	-- 	},
	-- }),
	format.prettierd,
	-- [java|type]script(react)?
	-- format.fixjson,
	lint.jsonlint,
	lint.eslint_d.with({
		condition = has_eslint_config,
		filetypes = add_svelte,
	}),
	format.eslint_d.with({
		condition = has_eslint_config,
	}),
	code_actions.eslint_d.with({
		condition = has_eslint_config,
		filetypes = add_svelte,
	}),
	-- C/C++
	format.clang_format,
	lint.cppcheck,
	-- Go
	format.gofumpt,
	-- format.golines,
	format.goimports,
	lint.golangci_lint,
	-- Lua
	lint.luacheck.with({
		extra_args = { '--globals', 'vim', 'describe' },
	}),
	-- lint.selene,
	format.stylua.with({
		extra_args = { '--indent-width', '2', '--quote-style', 'AutoPreferSingle', '--line-endings', 'Unix' },
	}),
	-- Python
	format.isort,
	format.black.with({ extra_args = { '-l', '80' } }),
	lint.flake8,
	-- Bash,zsh
	format.shfmt.with({ extra_args = { '-i', '2', '-ci' } }),
	lint.shellcheck,
	-- Markdown
	-- lint.vale,
	-- lint.write_good,
	format.markdownlint,
	-- Haskell
	format.fourmolu,
	-- Vim
	lint.vint,
	-- Yaml
	lint.yamllint,
	-- Misc
	lint.codespell,
	code_actions.gitsigns,
	-- b.hover.dictionary,
}
null_ls.setup({
	on_attach = function(_, bufnr)
		vim.keymap.set('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr })
		vim.keymap.set('n', ']g', vim.diagnostic.goto_next, { buffer = bufnr })
		vim.keymap.set('n', '[g', vim.diagnostic.goto_prev, { buffer = bufnr })
		print('LSP attached (null-ls)')
	end,
	sources = sources,
	diagnostics_format = '[#{c}] #{m} (#{s})',
	debug = true,
})
