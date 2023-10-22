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

return {
  'hrsh7th/nvim-cmp',
  event = { 'BufReadPost', 'BufNewFile' },
  dependencies = {
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    {
      'L3MON4D3/LuaSnip',
      version = '2.*',
      build = 'make install_jsregexp',
      dependencies = { 'saadparwaiz1/cmp_luasnip', 'rafamadriz/friendly-snippets' },
      config = function()
        local ls = require 'luasnip'
        ls.setup()
        vim.keymap.set({ 'i' }, '<C-K>', function()
          ls.expand()
        end, { silent = true })
        vim.keymap.set({ 'i', 's' }, '<C-L>', function()
          ls.jump(1)
        end, { silent = true })
        vim.keymap.set({ 'i', 's' }, '<C-J>', function()
          ls.jump(-1)
        end, { silent = true })
        require('luasnip.loaders.from_vscode').lazy_load()
      end,
    },
    {
      'onsails/lspkind.nvim',
      enabled = false,
      opts = require('sp.util').symbols.lsp_kinds,
      config = function(_, opts)
        require('lspkind').init(opts)
      end,
    },
    {
      'Exafunction/codeium.vim',
      event = 'InsertEnter',
      init = function()
        vim.g.codeium_disable_bindings = 1
      end,
      keys = {
        {
          '<A-g>',
          function()
            return vim.fn['codeium#Accept']()
          end,
          expr = true,
          mode = 'i',
        },
        {
          '<C-;>',
          function()
            return vim.fn['codeium#CycleCompletions'](1)
          end,
          expr = true,
          mode = 'i',
        },
        {
          '<C-,>',
          function()
            return vim.fn['codeium#CycleCompletions'](-1)
          end,
          expr = true,
          mode = 'i',
        },
        {
          '<A-x>',
          function()
            return vim.fn['codeium#Clear']()
          end,
          expr = true,
          mode = 'i',
        },
      },
    },
  },
  config = function()
    local cmp = require 'cmp'
    local types = require 'cmp.types'
    -- local context = require 'cmp.config.context'
    local compare = require 'cmp.config.compare'
    local sources = require 'cmp.config.sources'
    -- local defaults = require("cmp.config.default")()
    -- local lspkind = require('lspkind')
    -- local str = require("cmp.utils.str")
    -- local window = require 'cmp.config.window'
    local luasnip = require 'luasnip'

    vim.opt.completeopt = 'menu,menuone,noselect'
    local has_words_before = function()
      unpack = unpack or table.unpack
      local line, col = unpack(vim.api.nvim_win_get_cursor(0))
      return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
    end
    local feedkey = function(key, mode)
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
    end
    cmp.setup({
      snippet = {
        expand = function(args)
          -- vim.fn['vsnip#anonymous'](args.body) -- For `vsnip` user.
          luasnip.lsp_expand(args.body) -- For `luasnip` user.
        end,
      },
      window = {
        completion = {
          winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
          -- col_offset = -3,
          -- side_padding = 0,
        },
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-x><C-s>'] = cmp.mapping.complete({
          config = { sources = { { name = 'luasnip' } } },
        }),
        ['<C-x><C-f>'] = cmp.mapping.complete({
          config = { sources = { { name = 'path' } } },
        }),
        ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
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
        ['<C-e>'] = cmp.mapping({ i = cmp.mapping.close(), c = cmp.mapping.close() }),
        ['<CR>'] = cmp.mapping({
          i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
          c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
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
      }),
      sources = sources({
        { name = 'nvim_lua', priority = 100 },
        { name = 'nvim_lsp', priority = 100 },
        { name = 'nvim_lsp_signature_help', priority = 90 },
        { name = 'luasnip', priority = 50 },
      }, {
        { name = 'path', priority = 30 },
        {
          name = 'buffer',
          priority = 20,
          option = {
            keyword_length = 3,
            get_bufnrs = function()
              local bufIsSmall = function(bufnr)
                local max_filesize = 50 * 1024
                local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
                return ok and stats and stats.size < max_filesize
              end

              return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
            end,
          },
        },
      }),
      formatting = {
        format = function(_, vim_item)
          vim_item.kind = (require('sp.util').symbols.cmp_kinds[vim_item.kind] or '') .. vim_item.kind
          return vim_item
        end,
      },
      -- sorting = defaults.sorting,
      sorting = {
        priority_weight = 2,
        comparators = {
          -- deprio(types.lsp.CompletionItemKind.Snippet),
          deprio(types.lsp.CompletionItemKind.Text),
          compare.offset,
          compare.exact,
          compare.score,
          -- copied from cmp-under, but I don't think I need the plugin for this.
          -- I might add some more of my own.
          function(entry1, entry2)
            _G.entry_inspect = entry1
            local _, entry1_under = entry1.completion_item.label:find '^_+'
            local _, entry2_under = entry2.completion_item.label:find '^_+'
            entry1_under = entry1_under or 0
            entry2_under = entry2_under or 0
            if entry1_under > entry2_under then
              return false
            elseif entry1_under < entry2_under then
              return true
            end
          end,
          compare.kind,
          -- compare.locality,
          -- compare.sort_text,
          compare.recently_used,
          -- Try to put emmet towards the bottom
          function(entry1, entry2)
            local source_name = entry1.source
                and entry1.source.source
                and entry1.source.source.config
                and entry1.source.source.config.name
              or 'unknown'
            local target_name = entry2.source
                and entry2.source.source
                and entry2.source.source.config
                and entry2.source.source.config.name
              or 'unknown'
            if source_name == 'emmet_language_server' or source_name == 'emmet_ls' then
              return false
            end
            if target_name == 'emmet_language_server' or target_name == 'emmet_ls' then
              return true
            end
            return nil
          end,
        },
      },
      experimental = {
        ghost_text = false,
      },
    })

    cmp.setup.cmdline(':', {
      sources = sources({
        {
          name = 'cmdline',
          option = {
            ignore_cmds = { 'Man', '!' },
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
