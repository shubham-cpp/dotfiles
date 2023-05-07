return {
  'hrsh7th/nvim-cmp',
  event = {'BufReadPost', 'BufNewFile'},
  dependencies = {
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-buffer',
    'hrsh7th/vim-vsnip',
    'hrsh7th/cmp-vsnip',
    'rafamadriz/friendly-snippets',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'onsails/lspkind.nvim',
    {
      'Exafunction/codeium.vim',
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
    local compare = require 'cmp.config.compare'
    local sources = require 'cmp.config.sources'
    -- local window = require 'cmp.config.window'
    vim.opt.completeopt = 'menu,menuone,noselect'
    vim.g.vsnip_snippet_dir = vim.fn.stdpath 'config' .. '/vsnip'
    vim.g.vsnip_filetypes = {
      javascriptreact = { 'javascript' }, --, 'html' },
      typescriptreact = { 'typescript' }, --, 'html' },
      svelte = { 'javascript' },
      vue = { 'html' },
    }
    local has_words_before = function()
      local line, col = unpack(vim.api.nvim_win_get_cursor(0))
      return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
    end

    local feedkey = function(key, mode)
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
    end

    local t = function(str)
      return vim.api.nvim_replace_termcodes(str, true, true, true)
    end
    cmp.setup({
      snippet = {
        expand = function(args)
          vim.fn['vsnip#anonymous'](args.body) -- For `vsnip` user.
        end,
      },
      window = {
        completion = {
          winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
          col_offset = -3,
          side_padding = 0,
        },
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-x><C-s>'] = cmp.mapping.complete({
          config = { sources = { { name = 'vsnip' } } },
        }),
        ['<C-x><C-f>'] = cmp.mapping.complete({
          config = { sources = { { name = 'path' } } },
        }),
        ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        ['<C-n>'] = cmp.mapping({
          c = function()
            if cmp.visible() then
              cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
            else
              vim.api.nvim_feedkeys(t '<Down>', 'n', true)
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
              vim.api.nvim_feedkeys(t '<Up>', 'n', true)
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
          elseif vim.fn['vsnip#available'](1) == 1 then
            feedkey('<Plug>(vsnip-expand-or-jump)', '')
          elseif has_words_before() then
            cmp.complete()
          else
            fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
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
        { name = 'nvim_lsp',                priority = 1000 },
        { name = 'nvim_lsp_signature_help', priority = 700 },
        { name = 'nvim_lua',                priority = 900 },
        { name = 'vsnip',                   priority = 600 },
        { name = 'path',                    priority = 100 },
        {
          name = 'buffer',
          option = {
            keyword_length = 2,
            get_bufnrs = function()
              return vim.api.nvim_list_bufs()
            end,
          },
          priority = 300,
        },
        -- { name = 'buffer-lines' },
        -- { name = 'cmp_tabnine' },
      }),
      formatting = {
        fields = { 'kind', 'abbr', 'menu' },
        format = function(entry, vim_item)
          local kind = require('lspkind').cmp_format({ mode = 'symbol_text', maxwidth = 50 })(entry, vim_item)
          local strings = vim.split(kind.kind, '%s', { trimempty = true })
          kind.kind = ' ' .. strings[1] .. ' '
          kind.menu = '    (' .. strings[2] .. ')'

          return kind
        end,
        -- Without lspking
        --[[ format = function(entry, vim_item)
      -- Kind icons
      vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
      -- Source
      vim_item.menu = source_mapping[entry.source.name]
      return vim_item
    end, ]]
      },
      sorting = {
        priority_weight = 1, -- 2
        comparators = {
          compare.recently_used,
          compare.locality,
          compare.score,
          compare.offset,
          compare.order,
          compare.kind,
          compare.exact,
        },
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
      mapping = cmp.mapping.preset.cmdline({}),
    })

    cmp.setup.cmdline('/', {
      sources = sources({
        { name = 'nvim_lsp_signature_help' },
        { name = 'buffer',                 keyword_pattern = [=[[^[:blank:]].*]=] },
      }),
      mapping = cmp.mapping.preset.cmdline({}),
    })
  end,
}
