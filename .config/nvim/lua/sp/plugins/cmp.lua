local function serializeTable(val, name, skipnewlines, depth)
  skipnewlines = skipnewlines or false
  depth = depth or 0

  local tmp = string.rep(' ', depth)

  if name then
    tmp = tmp .. name .. ' = '
  end

  if type(val) == 'table' then
    tmp = tmp .. '{' .. (not skipnewlines and '\n' or '')

    for k, v in pairs(val) do
      tmp = tmp .. serializeTable(v, k, skipnewlines, depth + 1) .. ',' .. (not skipnewlines and '\n' or '')
    end

    tmp = tmp .. string.rep(' ', depth) .. '}'
  elseif type(val) == 'number' then
    tmp = tmp .. tostring(val)
  elseif type(val) == 'string' then
    tmp = tmp .. string.format('%q', val)
  elseif type(val) == 'boolean' then
    tmp = tmp .. (val and 'true' or 'false')
  else
    tmp = tmp .. '"[inserializeable datatype:' .. type(val) .. ']"'
  end

  return tmp
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

return {
  'hrsh7th/nvim-cmp',
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
      enabled = false,
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
    local defaults = require 'cmp.config.default'()
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
          luasnip.lsp_expand(args.body) -- For `luasnip` user.
        end,
      },
      window = {
        -- https://github.com/ChristianChiarulli/lvim/blob/master/lua/user/cmp.lua
        completion = {
          border = 'rounded',
          winhighlight = 'Normal:Pmenu,CursorLine:PmenuSel,FloatBorder:FloatBorder,Search:None',
          col_offset = -3,
          side_padding = 1,
          scrollbar = false,
          -- scrollbar = {
          --   position = 'inside',
          -- },
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
        ['<C-x><C-f>'] = cmp.mapping.complete({
          config = { sources = { { name = 'path' } } },
        }),
        ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i' }),
        ['<C-u>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i' }),
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
      }),
      sources = sources({
        { name = 'nvim_lsp', priority = 1000 },
        { name = 'nvim_lua', priority = 900 },
        { name = 'nvim_lsp_signature_help', priority = 800 },
        { name = 'luasnip', priority = 750 },
        -- }, {
        { name = 'path', priority = 500 },
        {
          name = 'buffer',
          priority = 300,
          option = {
            keyword_length = 2,
            get_bufnrs = function()
              local bufIsSmall = function(bufnr)
                --   local max_filesize = 50 * 1024
                --   local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
                --   return ok and stats and stats.size < max_filesize
                return vim.api.nvim_buf_line_count(bufnr) < 2000
              end

              return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
              -- return vim.api.nvim_list_bufs()
            end,
          },
        },
      }),
      formatting = {
        fields = { 'kind', 'abbr', 'menu' },
        format = function(entry, vim_item)
          vim_item.kind = require('sp.icons').kind[vim_item.kind]
          vim_item.menu = ({
            nvim_lsp = '',
            nvim_lua = '',
            luasnip = '',
            buffer = '',
            path = '',
            emoji = '',
            nvim_lsp_signature_help = '',
          })[entry.source.name]

          if vim.tbl_contains({ 'nvim_lsp' }, entry.source.name) then
            local duplicates = {
              buffer = 1,
              path = 1,
              nvim_lsp = 0,
              luasnip = 1,
            }

            local duplicates_default = 0

            vim_item.dup = duplicates[entry.source.name] or duplicates_default
          end
          return vim_item
        end,
      },
      -- sorting = defaults.sorting,
      sorting = {
        priority_weight = 2,
        comparators = {
          -- deprio(types.lsp.CompletionItemKind.Snippet),
          -- deprio(types.lsp.CompletionItemKind.Text),
          compare.exact,
          compare.score,
          compare.offset,
          compare.kind,
          compare.recently_used,
          compare.sort_text,
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
