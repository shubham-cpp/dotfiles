local function feedkey(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end
local function has_words_before()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end

local cmp = require 'cmp'
local sources = require 'cmp.config.sources'
local lspkind = require 'lspkind'
local snippet = 'luasnip'
local luasnip = require 'luasnip'
cmp.setup({
  snippet = {
    expand = function(args)
      -- vim.fn['vsnip#anonymous'](args.body)
      luasnip.lsp_expand(args.body)
    end,
  },
  window = { completion = cmp.config.window.bordered(), documentation = cmp.config.window.bordered() },

  duplicates = {
    nvim_lsp = 1,
    lazydev = 1,
    [snippet] = 1,
    cmp_tabnine = 1,
    buffer = 1,
    path = 1,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping({
      i = cmp.mapping.scroll_docs(-4),
    }),
    ['<C-u>'] = cmp.mapping({
      i = cmp.mapping.scroll_docs(4),
    }),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping({
      i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
      c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
    }),
    ['<C-y>'] = cmp.mapping({
      i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
      c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
    }),
    ['<C-j>'] = cmp.mapping({
      i = function(fallback)
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end,
    }),
    ['<C-k>'] = cmp.mapping({
      i = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end,
    }),
    ['<C-n>'] = cmp.mapping({
      c = function()
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        else
          feedkey('<Down>', 'n')
        end
      end,
      i = function(fallback)
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end,
    }),
    ['<C-p>'] = cmp.mapping({
      c = function()
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          feedkey('<Up>', 'n')
        end
      end,
      i = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end,
    }),
    ['<C-x><C-x>'] = cmp.mapping.complete({
      config = { sources = { { name = snippet } } },
    }),
    -- ['<C-x><C-f>'] = cmp.mapping.complete({
    --   config = { sources = { { name = 'path' } } },
    -- }),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
        -- elseif vim.snippet.active({ direction = 1 }) then
        -- return '<cmd>lua vim.snippet.jump(1)<cr>'
        -- vim.snippet.jump(1)
        -- elseif vim.fn['vsnip#available'](1) == 1 then
        --   feedkey('<Plug>(vsnip-expand-or-jump)', '')
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
      end
    end, { 'i', 's' }),

    ['<S-Tab>'] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
        -- elseif vim.snippet.active({ direction = -1 }) then
        -- return '<cmd>lua vim.snippet.jump(-1)<cr>'
        --   vim.snippet.jump(-1)
        -- elseif vim.fn['vsnip#jumpable'](-1) == 1 then
        --   feedkey('<Plug>(vsnip-jump-prev)', '')
      end
    end, { 'i', 's' }),
  },
  sources = sources({
    {
      name = 'nvim_lsp',
      priority = 1000,
      max_item_count = 12,
      entry_filter = function(entry)
        vim.print('source name = ', entry.source:get_debug_name())
        if
          (
            entry:get_kind() == require('cmp.types').lsp.CompletionItemKind.Text
            or entry:get_kind() == require('cmp.types').lsp.CompletionItemKind.Snippet
          ) and entry.source:get_debug_name() == 'nvim_lsp:emmet_language_server'
        then
          return false
        end
        return true
      end,
    },
    { name = 'lazydev', priority = 850 },
    { name = snippet, priority = 750 },
    {
      name = 'buffer',
      max_item_count = 10,
      option = {
        keyword_length = 2,
        get_bufnrs = function()
          local bufIsSmall = function(bufnr)
            return vim.api.nvim_buf_line_count(bufnr) < 2000
          end
          return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
        end,
      },
      priority = 400,
    },
    { name = 'path', priority = 250 },
  }),
  -- sorting = {
  --   priority_weight = 2,
  --   comparators = {
  --     cmp.config.compare.offset,
  --     cmp.config.compare.exact,
  --     cmp.config.compare.score,
  --     cmp.config.compare.recently_used,
  --     cmp.config.compare.locality,
  --     cmp.config.compare.kind,
  --     cmp.config.compare.sort_text,
  --     cmp.config.compare.length,
  --     cmp.config.compare.order,
  --   },
  -- },
  formatting = {
    fields = { 'kind', 'abbr', 'menu' },
    format = lspkind.cmp_format({ with_text = true, maxwidth = 50 }),
  },
})
cmp.setup.cmdline(':', {
  sources = sources({
    {
      name = 'cmdline',
      option = {
        ignore_cmds = { 'Man', '!', 'find', 'fin' },
      },
    },
    { name = 'path' },
  }),
  matching = { disallow_symbol_nonprefix_matching = false },
  mapping = cmp.mapping.preset.cmdline(),
  formatting = { fields = { 'abbr' } },
  window = { completion = cmp.config.window.bordered({ col_offset = 0 }) },
})

cmp.setup.cmdline({ '/', '?' }, {
  sources = sources({
    { name = 'buffer' },
  }),
  mapping = cmp.mapping.preset.cmdline(),
  formatting = { fields = { 'abbr' } },
  window = { completion = cmp.config.window.bordered({ col_offset = 0 }) },
})
