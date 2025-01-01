local function feedkey(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end
local function has_words_before()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end

local function deprio(kind)
  return function(e1, e2)
    if e1:get_kind() == kind then
      return false
    end
    if e2:get_kind() == kind then
      return true
    end
  end
end

--- detect if the current completion item is an emmet completion item
--- @param entry cmp.Entry
--- @return boolean
local function isEmmet(entry)
  return (
    entry:get_kind() == require('cmp.types').lsp.CompletionItemKind.Text
    or entry:get_kind() == require('cmp.types').lsp.CompletionItemKind.Snippet
  )
    and (
      entry.source:get_debug_name() == 'nvim_lsp:emmet_language_server'
      or entry.source:get_debug_name() == 'nvim_lsp:emmet_ls'
    )
end

---@type LazySpec
return {
  'iguanacucumber/magazine.nvim',
  name = 'nvim-cmp', -- Otherwise highlighting gets messed up
  enabled = true,
  event = 'BufReadPre',
  dependencies = {
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    -- 'hrsh7th/cmp-path',
    'https://codeberg.org/FelipeLema/cmp-async-path',
    'hrsh7th/cmp-cmdline',
    'lukas-reineke/cmp-rg',
    -- 'lukas-reineke/cmp-under-comparator',
    'octaltree/cmp-look',
    'saadparwaiz1/cmp_luasnip',
    {
      'L3MON4D3/LuaSnip',
      build = 'make install_jsregexp',
      dependencies = { 'rafamadriz/friendly-snippets' },
      event = 'User BaseFile',
      opts = { history = true, delete_check_events = 'TextChanged', region_check_events = 'CursorMoved' },
      config = function(_, opts)
        if opts then
          require('luasnip').config.setup(opts)
        end

        vim.tbl_map(function(type)
          require('luasnip.loaders.from_' .. type).lazy_load()
        end, { 'vscode', 'snipmate', 'lua' })

        local extends = {
          typescript = { 'tsdoc' },
          javascript = { 'jsdoc' },
          lua = { 'luadoc' },
          python = { 'pydoc' },
          rust = { 'rustdoc' },
          cs = { 'csharpdoc' },
          java = { 'javadoc' },
          c = { 'cdoc' },
          cpp = { 'cppdoc' },
          php = { 'phpdoc' },
          kotlin = { 'kdoc' },
          ruby = { 'rdoc' },
          sh = { 'shelldoc' },
        }
        -- friendly-snippets - enable standardized comments snippets
        for ft, snips in pairs(extends) do
          require('luasnip').filetype_extend(ft, snips)
        end
      end,
    },
    'onsails/lspkind.nvim',
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      opts = {
        library = { 'lazy.nvim', { path = 'luvit-meta/library', words = { 'vim%.uv' } } },
      },
    },
    { 'Bilal2453/luvit-meta', lazy = true },
  },
  config = function()
    local cmp = require 'cmp'
    local types = require 'cmp.types'
    local sources = require 'cmp.config.sources'
    local compare = require 'cmp.config.compare'
    local lspkind = require 'lspkind'
    local snippet = 'luasnip'
    local luasnip = require 'luasnip'

    cmp.setup({
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      experimental = { ghost_text = false, native_menu = false },
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
        ['<C-d>'] = cmp.mapping({ i = cmp.mapping.scroll_docs(-4) }),
        ['<C-u>'] = cmp.mapping({ i = cmp.mapping.scroll_docs(4) }),
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
                  return isEmmet(entry)
                end,
              },
            },
          },
        }),
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
            fallback()
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
            return not isEmmet(entry)
          end,
        },
        { name = 'lazydev', priority = 95 },
        { name = snippet, priority = 90 },
        {
          name = 'buffer',
          max_item_count = 10,
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
          keyword_length = 3,
          max_item_count = 10,
          priority_weight = 50,
          option = {
            additional_arguments = '--smart-case --hidden',
            set_filetype = true,
            marker = ' ❰❰❰',
          },
          -- entry_filter = function(entry)
          --   return not entry.exact
          -- end,
        },
        -- { name = 'path', priority = 50 },
        { name = 'async_path', priority = 50, option = { trailing_slash = false } },
        {
          name = 'look',
          keyword_length = 5,
          priority = 30,
          option = {
            convert_case = true,
            loud = true,
            --dict = '/usr/share/dict/words'
          },
        },
      }),
      sorting = {
        priority_weight = 100,
        comparators = {
          deprio(types.lsp.CompletionItemKind.Text),
          compare.exact,
          compare.offset,
          compare.recently_used,
          compare.score,
          compare.kind,
          compare.length,
          compare.locality,
          compare.order,
          compare.sort_text,
        },
      },
      ---@diagnostic disable-next-line: missing-fields
      formatting = {
        fields = { 'kind', 'abbr', 'menu' },
        format = lspkind.cmp_format({
          mode = 'symbol',
          maxwidth = 50,
          ellipsis_char = '...',
          symbol_map = { Copilot = '' },
        }),
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
  end,
}
