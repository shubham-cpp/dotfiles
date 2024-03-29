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
    -- 'hrsh7th/cmp-nvim-lua',
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
        ls.config.setup({
          history = true,
          delete_check_events = 'TextChanged',
          region_check_events = 'CursorMoved',
        })
        vim.tbl_map(function(type)
          require('luasnip.loaders.from_' .. type).lazy_load()
        end, { 'vscode', 'snipmate', 'lua' })

        require('luasnip.loaders.from_vscode').lazy_load({
          paths = { vim.fn.expand '~/Documents/dotfiles/.config/astronvim/lua/user/snippets' },
        }) -- load snippets paths
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
  },
  config = function()
    local cmp = require 'cmp'
    local compare = require 'cmp.config.compare'
    local sources = require 'cmp.config.sources'
    local defaults = require 'cmp.config.default'()
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
          luasnip.lsp_expand(args.body) -- For `luasnip` user.
        end,
      },
      -- window = {
      --   documentation = cmp.config.window.bordered({ winhighlight = 'FloatBorder:Todo', side_padding = 0 }),
      --   completion = {
      --     winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None',
      --     -- winhighlight = "Normal:Normal,FloatBorder:BorderBG,CursorLine:PmenuSel,Search:None",
      --   },
      -- },
      window = {
        completion = {
          border = 'rounded',
          winhighlight = 'Normal:Pmenu,CursorLine:PmenuSel,FloatBorder:FloatBorder,Search:None',
          col_offset = -3,
          side_padding = 1,
          scrollbar = false,
          scrolloff = 8,
        },
        documentation = {
          border = 'rounded',
          winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,Search:None',
        },
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-x><C-x>'] = cmp.mapping.complete({
          config = { sources = { { name = 'luasnip' } } },
        }),
        ['<C-x><C-b>'] = cmp.mapping.complete({
          config = { sources = { { name = 'buffer' } } },
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
              cmp.select_next_item()
            -- elseif luasnip.expand_or_jumpable() then
            elseif luasnip.expand_or_locally_jumpable() then
              luasnip.expand_or_jump()
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end,
          s = function(fallback)
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
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end,
          s = function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
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
          elseif luasnip.expandable() then
            luasnip.expand()
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
        { name = 'nvim_lsp', priority = 1000 },
        -- { name = 'nvim_lua', priority = 950 },
        { name = 'nvim_lsp_signature_help', priority = 900 },
        { name = 'luasnip', priority = 750 },
        -- }, {
        {
          name = 'buffer',
          priority = 500,
          option = {
            keyword_length = 2,
            get_bufnrs = function()
              local bufIsSmall = function(bufnr)
                -- local max_filesize = 50 * 1024
                -- local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
                -- return ok and stats and stats.size < max_filesize
                return vim.api.nvim_buf_line_count(bufnr) < 1500
              end

              return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
              -- return vim.api.nvim_list_bufs()
            end,
          },
        },
        { name = 'path', priority = 700 },
      }),
      formatting = {
        fields = { 'kind', 'abbr', 'menu' },
        format = function(entry, vim_item)
          vim_item.kind = require('sp.util').symbols.kind[vim_item.kind]
          vim_item.menu = ({
            nvim_lsp = '',
            nvim_lua = '',
            luasnip = '',
            buffer = '',
            path = '',
            emoji = '',
          })[entry.source.name]
          return vim_item
        end,
      },
      -- sorting = defaults.sorting,
      sorting = {
        -- priority_weight = 2,
        comparators = {
          -- deprio(types.lsp.CompletionItemKind.Snippet),
          -- deprio(types.lsp.CompletionItemKind.Text),
          cmp.config.compare.offset,
          cmp.config.compare.exact,
          cmp.config.compare.score,
          cmp.config.compare.recently_used,
          cmp.config.compare.locality,
          -- require("cmp-under-comparator").under,
          cmp.config.compare.kind,
          cmp.config.compare.sort_text,
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
          --[[
          deprio(types.lsp.CompletionItemKind.Text),
          cmp.config.compare.offset,
          cmp.config.compare.exact,
          cmp.config.compare.score,
          cmp.config.compare.recently_used,
          cmp.config.compare.locality,
          cmp.config.compare.kind,
          cmp.config.compare.sort_text,
          cmp.config.compare.length,
          cmp.config.compare.order,
          --]]
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
    -- -- gray
    -- vim.api.nvim_set_hl(0, 'CmpItemAbbrDeprecated', { bg = 'NONE', strikethrough = true, fg = '#808080' })
    -- -- blue
    -- vim.api.nvim_set_hl(0, 'CmpItemAbbrMatch', { bg = 'NONE', fg = '#569CD6' })
    -- vim.api.nvim_set_hl(0, 'CmpItemAbbrMatchFuzzy', { link = 'CmpIntemAbbrMatch' })
    -- -- light blue
    -- vim.api.nvim_set_hl(0, 'CmpItemKindVariable', { bg = 'NONE', fg = '#9CDCFE' })
    -- vim.api.nvim_set_hl(0, 'CmpItemKindInterface', { link = 'CmpItemKindVariable' })
    -- vim.api.nvim_set_hl(0, 'CmpItemKindText', { link = 'CmpItemKindVariable' })
    -- -- pink
    -- vim.api.nvim_set_hl(0, 'CmpItemKindFunction', { bg = 'NONE', fg = '#C586C0' })
    -- vim.api.nvim_set_hl(0, 'CmpItemKindMethod', { link = 'CmpItemKindFunction' })
    -- -- front
    -- vim.api.nvim_set_hl(0, 'CmpItemKindKeyword', { bg = 'NONE', fg = '#D4D4D4' })
    -- vim.api.nvim_set_hl(0, 'CmpItemKindProperty', { link = 'CmpItemKindKeyword' })
    -- vim.api.nvim_set_hl(0, 'CmpItemKindUnit', { link = 'CmpItemKindKeyword' })
    -- vim.api.nvim_set_hl(0, 'PmenuSel', { underline = true ,bold=true, bg="#434442" })
    -- vim.api.nvim_set_hl(0, 'PmenuKindSel', { underline = true ,bold=true, bg="darkyellow"})
    -- vim.api.nvim_set_hl(0, 'PmenuExtraSel', { underline = true ,bold=true, bg="darkyellow"})
    --[[
PmenuSel = { bg = "#282C34", fg = "NONE" },
  Pmenu = { fg = "#C5CDD9", bg = "#22252A" },
    --]]
  end,
}
