local cmp = require('cmp')
local compare = require('cmp.config.compare')
local sources = require('cmp.config.sources')
local window = require('cmp.config.window')
local lspkind = require('lspkind')
local g = vim.g
local o = vim.opt

o.completeopt = 'menu,menuone,noselect'
g.vsnip_snippet_dir = vim.fn.stdpath('config') .. '/vsnip'
g.vsnip_filetypes = {
	javascriptreact = { 'javascript', 'html' },
	typescriptreact = { 'typescript', 'html' },
	svelte = { 'javascript' },
	vue = { 'html' },
}
local source_mapping = {
	buffer = '[Buffer]',
	nvim_lsp = '[LSP]',
	nvim_lua = '[Lua]',
	cmp_tabnine = '[TN]',
	path = '[Path]',
	vsnip = '[Vsnip]',
}

local has_words_before = function()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
end

local feedkey = function(key, mode)
	vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end

cmp.setup({
	snippet = {
		expand = function(args)
			vim.fn['vsnip#anonymous'](args.body) -- For `vsnip` user.
		end,
	},
	-- window = {
	--  completion    = window.bordered(),
	--  documentation = window.bordered(),
	-- },
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
		-- ['<C-e>'] = cmp.mapping.abort(),
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
		{ name = 'cmp_tabnine' },
		{ name = 'copilot' },
		{ name = 'path' },
		-- { name = "org" },
		{ name = 'neorg' },
		{
			name = 'buffer',
			option = {
				keyword_length = 2,
				get_bufnrs = function()
					return vim.api.nvim_list_bufs()
				end,
			},
		},
		{ name = 'spell', option = { keyword_length = 4 } },
	}),
	formatting = {
		-- format = lspkind.cmp_format({
		-- 	with_text = true, -- do not show text alongside icons
		-- 	maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
		-- }),
		format = function(entry, vim_item)
			vim_item.kind = lspkind.presets.default[vim_item.kind]
			local menu = source_mapping[entry.source.name]
			if entry.source.name == 'cmp_tabnine' then
				if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
					menu = entry.completion_item.data.detail .. ' ' .. menu
				end
				vim_item.kind = ' '
			elseif entry.source == 'copilot' then
				if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
					menu = entry.completion_item.data.detail .. ' ' .. menu
				end
				vim_item.kind = ' '
			end
			vim_item.menu = menu
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
