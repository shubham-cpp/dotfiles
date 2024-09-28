local cmp = require 'cmp'
local types = require 'cmp.types'
local sources = require 'cmp.config.sources'
local compare = require 'cmp.config.compare'
local lspkind = require 'lspkind'
local snippet = 'luasnip'
local luasnip = require 'luasnip'

local function feedkey(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end
local function has_words_before()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end
local priority_map = {
  [types.lsp.CompletionItemKind.Snippet] = 0, -- top
  [types.lsp.CompletionItemKind.Keyword] = 0, -- top
  [types.lsp.CompletionItemKind.EnumMember] = 1,
  [types.lsp.CompletionItemKind.Variable] = types.lsp.CompletionItemKind.Method,
  -- [types.lsp.CompletionItemKind.Variable] = 2,
  [types.lsp.CompletionItemKind.Text] = 100,
}

local kind = function(entry1, entry2)
  local kind1 = entry1:get_kind()
  local kind2 = entry2:get_kind()
  kind1 = priority_map[kind1] or kind1
  kind2 = priority_map[kind2] or kind2
  if kind1 ~= kind2 then
    if kind1 == types.lsp.CompletionItemKind.Snippet then
      return true
    end
    if kind2 == types.lsp.CompletionItemKind.Snippet then
      return false
    end
    local diff = kind1 - kind2
    if diff < 0 then
      return true
    elseif diff > 0 then
      return false
    end
  end
end

cmp.setup({
  snippet = {
    expand = function(args)
      -- vim.fn['vsnip#anonymous'](args.body)
      luasnip.lsp_expand(args.body)
    end,
  },
  experimental = {
    ghost_text = false,
    native_menu = false,
  },
  window = { completion = cmp.config.window.bordered(), documentation = cmp.config.window.bordered() },
  -- window = {
  --   documentation = cmp.config.window.bordered({
  --     winhighlight = 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None',
  --   }),
  -- },

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
    ['<C-x><C-e>'] = cmp.mapping.complete({
      config = {
        sources = {
          {
            name = 'nvim_lsp',
            entry_filter = function(entry)
              vim.print(entry.source:get_debug_name())
              if
                (
                  entry:get_kind() == require('cmp.types').lsp.CompletionItemKind.Text
                  or entry:get_kind() == require('cmp.types').lsp.CompletionItemKind.Snippet
                ) and entry.source:get_debug_name() == 'nvim_lsp:emmet_language_server'
              then
                return true
              end
              return false
            end,
          },
        },
      },
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
      priority = 100,
      max_item_count = 20,
      entry_filter = function(entry)
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
    { name = 'lazydev', priority = 95 },
    { name = snippet, priority = 90 },
    {
      name = 'buffer',
      max_item_count = 10,
      entry_filter = function(entry)
        return not entry.exact
      end,
      option = {
        keyword_length = 2,
        -- get_bufnrs = function()
        --   local bufIsSmall = function(bufnr)
        --     return vim.api.nvim_buf_line_count(bufnr) < 2000
        --   end
        --   return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
        -- end,
      },
      priority = 40,
    },
    {
      name = 'rg',
      keyword_length = 4,
      max_item_count = 10,
      priority_weight = 50,
      option = {
        additional_arguments = '--smart-case --hidden',
        set_filetype = true,
        marker = ' ❰❰❰',
      },
      entry_filter = function(entry)
        return not entry.exact
      end,
    },
    { name = 'path', priority = 50 },
    {
      name = 'look',
      keyword_length = 4,
      priority = 30,
      option = {
        convert_case = true,
        loud = true,
        --dict = '/usr/share/dict/words'
      },
    },
  }),
  matching = {
    disallow_fuzzy_matching = false, -- fmodify -> fnamemodify
    disallow_fullfuzzy_matching = true,
    disallow_partial_fuzzy_matching = true,
    disallow_partial_matching = false, -- fb -> foo_bar
    disallow_prefix_unmatching = true, -- bar -> foo_bar
    disallow_symbol_nonprefix_matching = true, -- _bar -> foo_bar
  },
  sorting = {
    priority_weight = 10,
    -- comparators = {
    --   cmp.config.compare.offset,
    --   cmp.config.compare.exact,
    --   cmp.config.compare.score,
    --   require('cmp-under-comparator').under,
    --   kind,
    --   cmp.config.compare.recently_used,
    --   cmp.config.compare.locality,
    --   cmp.config.compare.sort_text,
    --   cmp.config.compare.length,
    --   cmp.config.compare.order,
    -- },
    comparators = {
      compare.offset,
      compare.exact,
      function(entry1, entry2) -- sort by length ignoring "=~"
        local len1 = string.len(string.gsub(entry1.completion_item.label, '[=~()_]', ''))
        local len2 = string.len(string.gsub(entry2.completion_item.label, '[=~()_]', ''))
        if len1 ~= len2 then
          return len1 - len2 < 0
        end
      end,
      require('cmp-under-comparator').under,
      compare.recently_used, ---@diagnostic disable-line
      kind,
      function(entry1, entry2) -- score by lsp, if available
        local t1 = entry1.completion_item.sortText
        local t2 = entry2.completion_item.sortText
        if t1 ~= nil and t2 ~= nil and t1 ~= t2 then
          return t1 < t2
        end
      end,
      compare.score,
      compare.order,
    },
  },
  ---@diagnostic disable-next-line: missing-fields
  formatting = {
    fields = { 'kind', 'abbr', 'menu' },
    format = lspkind.cmp_format({
      mode = 'symbol',
      maxwidth = 50,
      ellipsis_char = '...',
      symbol_map = {
        Copilot = '',
      },
    }),
    -- fields = { 'kind', 'abbr', 'menu' },
    -- format = lspkind.cmp_format({ with_text = true, maxwidth = 50 }),
    -- format = function(entry, item)
    --   local color_item = require('nvim-highlight-colors').format(entry, { kind = item.kind })
    --   item = lspkind.cmp_format({
    --     -- any lspkind format settings here
    --   })(entry, item)
    --   if color_item.abbr_hl_group then
    --     item.kind_hl_group = color_item.abbr_hl_group
    --     item.kind = color_item.abbr
    --   end
    --   return item
    -- end,
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
