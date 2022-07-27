local ok, cmp = pcall(require, 'cmp')
if not ok then
	return
end
local compare = require('cmp.config.compare')
local sources = require('cmp.config.sources')
local window = require('cmp.config.window')
vim.opt.completeopt = 'menu,menuone,noselect'
vim.g.vsnip_snippet_dir = vim.fn.stdpath('config') .. '/vsnip'
vim.g.vsnip_filetypes = {
	javascriptreact = { 'javascript', 'html' },
	typescriptreact = { 'typescript', 'html' },
	svelte = { 'javascript' },
	vue = { 'html' },
}
local has_words_before = function()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
end

local feedkey = function(key, mode)
	vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end

local source_mapping = {
	buffer = '[Buffer]',
	nvim_lsp = '[LSP]',
	nvim_lua = '[Lua]',
	cmp_tabnine = '[TN]',
	path = '[Path]',
	vsnip = '[Vsnip]',
}
local kind_icons = {
	Text = '',
	Method = '',
	Function = '',
	Constructor = '',
	Field = '',
	Variable = '',
	Class = 'ﴯ',
	Interface = '',
	Module = '',
	Property = 'ﰠ',
	Unit = '',
	Value = '',
	Enum = '',
	Keyword = '',
	Snippet = '',
	Color = '',
	File = '',
	Reference = '',
	Folder = '',
	EnumMember = '',
	Constant = '',
	Struct = '',
	Event = '',
	Operator = '',
	TypeParameter = '',
	cmp_tabnine = ' ',
}
cmp.setup({
	snippet = {
		expand = function(args)
			vim.fn['vsnip#anonymous'](args.body) -- For `vsnip` user.
		end,
	},
	mapping = cmp.mapping.preset.insert({
		['<C-x><C-s>'] = cmp.mapping.complete({
			config = { sources = { { name = 'vsnip' } } },
		}),
		['<C-x><C-f>'] = cmp.mapping.complete({
			config = { sources = { { name = 'path' } } },
		}),
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-n>'] = cmp.mapping(function()
			if cmp.visible() then
				cmp.select_next_item()
			else
				cmp.complete()
			end
		end, { 'i', 'c' }),
		['<C-p>'] = cmp.mapping(function()
			if cmp.visible() then
				cmp.select_prev_item()
			else
				cmp.complete()
			end
		end, { 'i', 'c' }),
		['<C-e>'] = cmp.mapping({
			i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
		['<CR>'] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
		['<Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif vim.fn['vsnip#available'](1) == 1 then
				feedkey('<Plug>(vsnip-expand-or-jump)', '')
			elseif has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end, { 'i', 's' }),

		['<S-Tab>'] = cmp.mapping(function()
			if cmp.visible() then
				cmp.select_prev_item()
			elseif vim.fn['vsnip#jumpable'](-1) == 1 then
				feedkey('<Plug>(vsnip-jump-prev)', '')
			end
		end, { 'i', 's' }),
	}),
	sources = sources({
		{ name = 'nvim_lsp' },
		{ name = 'nvim_lsp_signature_help' },
		{ name = 'nvim_lua' },
		{ name = 'vsnip' },
		{ name = 'path' },
		{ name = 'cmp_tabnine' },
		{
			name = 'buffer',
			option = {
				keyword_length = 2,
				get_bufnrs = function()
					return vim.api.nvim_list_bufs()
				end,
			},
		},
		-- { name = 'spell', option = { keyword_length = 4 } },
	}),
	formatting = {
		format = function(entry, vim_item)
			-- Kind icons
			vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
			-- Source
			vim_item.menu = source_mapping[entry.source.name]
			return vim_item
		end,
	},
	-- sorting = {
	-- 	priority_weight = 2,
	-- 	comparators = {
	-- 		require("cmp_tabnine.compare"),
	-- 		compare.offset,
	-- 		compare.recently_used,
	-- 		compare.exact,
	-- 		compare.score,
	-- 		compare.kind,
	-- 		compare.sort_text,
	-- 		compare.length,
	-- 		compare.order,
	-- 	},
	-- },
})

cmp.setup.cmdline(':', {
	sources = sources({
		{ name = 'cmdline' },
		{ name = 'path' },
	}),
	mapping = cmp.mapping.preset.cmdline({}),
})

cmp.setup.cmdline('/', {
	sources = sources({
		{ name = 'nvim_lsp_signature_help' },
		{ name = 'buffer', keyword_pattern = [=[[^[:blank:]].*]=] },
	}),
	mapping = cmp.mapping.preset.cmdline({}),
})
