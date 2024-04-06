local function feedkey(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end
local function has_words_before()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end

return {
  'hrsh7th/nvim-cmp',
  event = 'BufReadPost', --"InsertEnter",
  dependencies = {
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    {
      'L3MON4D3/LuaSnip',
      version = 'v2.*',
      run = 'make install_jsregexp',
    },
    'saadparwaiz1/cmp_luasnip',
    'rafamadriz/friendly-snippets',
    'onsails/lspkind.nvim',
  },
  config = function()
    local cmp = require 'cmp'
    local lspkind = require 'lspkind'
    local luasnip = require 'luasnip'
    luasnip.config.setup({})
    require('luasnip.loaders.from_vscode').lazy_load()
    require('luasnip.loaders.from_snipmate').lazy_load()

    cmp.setup({
      snippet = {
        expand = function(args)
          require('luasnip').lsp_expand(args.body)
        end,
      },
      mapping = {
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-u>'] = cmp.mapping.scroll_docs(4),
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
          config = { sources = { { name = 'luasnip' } } },
        }),
        ['<C-x><C-f>'] = cmp.mapping.complete({
          config = { sources = { { name = 'path' } } },
        }),
        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          -- elseif luasnip.expand_or_jumpable() then
          elseif luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<C-f>'] = cmp.mapping(function(fallback)
          if luasnip.jumpable(1) then
            luasnip.jump(1)
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<C-b>'] = cmp.mapping(function(fallback)
          if luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { 'i', 's' }),
      },
      sources = {
        { name = 'nvim_lsp', priority = 1000 },
        { name = 'luasnip', priority = 700 },
        {
          name = 'buffer',
          option = {
            keyword_length = 2,
            get_bufnrs = function()
              local bufIsSmall = function(bufnr)
                return vim.api.nvim_buf_line_count(bufnr) < 2000
              end
              return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
              -- return vim.api.nvim_list_bufs()
            end,
          },
          priority = 300,
        },
        { name = 'path', priority = 200 },
      },
      formatting = {
        format = lspkind.cmp_format({ with_text = true, maxwidth = 50 }),
      },
    })

    local sources = require 'cmp.config.sources'
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
      mapping = cmp.mapping.preset.cmdline(),
      formatting = { fields = { 'abbr' } },
      window = { completion = cmp.config.window.bordered({ col_offset = 0 }) },
    })

    cmp.setup.cmdline({ '/', '?' }, {
      sources = sources({
        { name = 'buffer' },
        -- { name = 'nvim_lsp_signature_help' },
        -- { name = 'buffer',                 keyword_pattern = [=[[^[:blank:]].*]=] },
      }),
      mapping = cmp.mapping.preset.cmdline(),
      formatting = { fields = { 'abbr' } },
      window = { completion = cmp.config.window.bordered({ col_offset = 0 }) },
    })
  end,
}
