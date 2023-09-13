local cmp_kinds = {
  Text = '  ',
  Method = '  ',
  Function = '  ',
  Field = '  ',
  Variable = '  ',
  Interface = '  ',
  Module = '  ',
  Property = '  ',
  Value = '  ',
  Enum = '  ',
  Keyword = '  ',
  Color = '  ',
  File = '  ',
  Folder = '  ',
  EnumMember = '  ',
  Constant = '  ',
  Struct = '  ',
  Event = '  ',
  Operator = '  ',
  TypeParameter = '  ',
  Array = "󰅪",
  Boolean = "⊨",
  Class = "󰌗",
  Constructor = "",
  Key = "󰌆",
  Namespace = "󰅪",
  Null = "NULL",
  Number = "#",
  Object = "󰀚",
  Package = "󰏗",
  Reference = "",
  Snippet = "",
  String = "󰀬",
  Unit = "",
}
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
    'hrsh7th/vim-vsnip',
    'hrsh7th/cmp-vsnip',
    'rafamadriz/friendly-snippets',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    {
      'onsails/lspkind.nvim',
      enabled = false,
      opts = {
        mode = "symbol",
        symbol_map = {
          Array = "󰅪",
          Boolean = "⊨",
          Class = "󰌗",
          Constructor = "",
          Key = "󰌆",
          Namespace = "󰅪",
          Null = "NULL",
          Number = "#",
          Object = "󰀚",
          Package = "󰏗",
          Property = "",
          Reference = "",
          Snippet = "",
          String = "󰀬",
          TypeParameter = "󰊄",
          Unit = "",
        },
        menu = {},
      },
      config = function(_, opts)
        require("lspkind").init(opts)
      end
    },
    {
      'Exafunction/codeium.vim',
      event = "InsertEnter",
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

    vim.opt.completeopt = 'menu,menuone,noselect'
    vim.g.vsnip_snippet_dir = vim.fn.stdpath 'config' .. '/vsnip'
    vim.g.vsnip_filetypes = {
      javascriptreact = { 'javascript' }, --, 'html' },
      typescriptreact = { 'typescript' }, --, 'html' },
      svelte = { 'javascript' },
      vue = { 'html' },
    }
    local has_words_before = function()
      unpack = unpack or table.unpack
      local line, col = unpack(vim.api.nvim_win_get_cursor(0))
      return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
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
      window = {
        completion = {
          winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
          -- col_offset = -3,
          -- side_padding = 0,
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
              -- vim.api.nvim_feedkeys(t '<Down>', 'n', true)
              feedkey("<Down>", "n")
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
              -- vim.api.nvim_feedkeys(t '<Up>', 'n', true)
              feedkey("<Up>", "n")
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
          elseif vim.fn["vsnip#available"](1) == 1 then
            feedkey("<Plug>(vsnip-expand-or-jump)", "")
          elseif has_words_before() then
            cmp.complete()
          else
            fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function()
          if cmp.visible() then
            cmp.select_prev_item()
          elseif vim.fn["vsnip#jumpable"](-1) == 1 then
            feedkey("<Plug>(vsnip-jump-prev)", "")
          end
        end, { 'i', 's' }),
      }),
      sources = sources({
        { name = 'nvim_lua',                 priority = 100 },
        { name = 'nvim_lsp',                 priority = 100 },
        { name = 'nvim_lsp_signature_help',  priority = 90 },
        { name = 'vsnip',                    priority = 50 },
        -- { name = 'buffer-lines' },
        -- { name = 'cmp_tabnine' },
      }, {
        { name = 'path', priority = 30 },
        {
          name = 'buffer'
          ,
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
          vim_item.kind = (cmp_kinds[vim_item.kind] or '') .. vim_item.kind
          return vim_item
        end,
      },
      -- sorting = {
      --   priority_weight = 2,
      --   comparators = {
      --     compare.exact,
      --     compare.scopes,
      --     compare.recently_used,
      --   },
      -- },
      -- sorting = defaults.sorting,
      sorting = {
        priority_weight = 2,
        comparators = {
          deprio(types.lsp.CompletionItemKind.Snippet),
          deprio(types.lsp.CompletionItemKind.Text),
          compare.offset,
          compare.exact,
          compare.score,
          -- copied from cmp-under, but I don't think I need the plugin for this.
          -- I might add some more of my own.
          function(entry1, entry2)
            local _, entry1_under = entry1.completion_item.label:find "^_+"
            local _, entry2_under = entry2.completion_item.label:find "^_+"
            entry1_under = entry1_under or 0
            entry2_under = entry2_under or 0
            if entry1_under > entry2_under then
              return false
            elseif entry1_under < entry2_under then
              return true
            end
          end,
          compare.kind,
          -- compare.sort_text,
          compare.recently_used,
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
      formatting = { fields = { "abbr" } },
      window = { completion = cmp.config.window.bordered({ col_offset = 0 }) },
    })

    cmp.setup.cmdline({ '/', '?' }, {
      sources = sources({
        { name = "buffer" },
        -- { name = 'nvim_lsp_signature_help' },
        -- { name = 'buffer',                 keyword_pattern = [=[[^[:blank:]].*]=] },
      }),
      mapping = cmp.mapping.preset.cmdline(),
      formatting = { fields = { "abbr" } },
      window = { completion = cmp.config.window.bordered({ col_offset = 0 }) },
    })
  end,
}
