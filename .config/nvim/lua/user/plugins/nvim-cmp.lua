local function has_words_before()
  local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end
local function is_visible(cmp)
  return cmp.core.view:visible() or vim.fn.pumvisible() == 1
end

local function get_icon_provider()
  local _, mini_icons = pcall(require, 'mini.icons')
  if _G.MiniIcons then
    return function(kind)
      return mini_icons.get('lsp', kind or '')
    end
  end
  local lspkind_avail, lspkind = pcall(require, 'lspkind')
  if lspkind_avail then
    return function(kind)
      return lspkind.symbolic(kind, { mode = 'symbol' })
    end
  end
end

---@param kind lsp.CompletionItemKind | number
---@return cmp.ComparatorFunction
local function deprio(kind)
  return function(e1, e2)
    -- vim.inspect()
    if e1:get_kind() == kind then
      return false
    end
    if e2:get_kind() == kind then
      return true
    end
  end
end

---@return cmp.ComparatorFunction
local function lower_emmet()
  return function(e1, e2)
    local is_e1_emmet = e1.source:get_debug_name() == 'nvim_lsp:emmet_ls'
      or e1.source:get_debug_name() == 'nvim_lsp:emmet_language_server'
    local is_e2_emmet = e2.source:get_debug_name() == 'nvim_lsp:emmet_ls'
      or e2.source:get_debug_name() == 'nvim_lsp:emmet_language_server'
    if is_e1_emmet then
      return false
    end
    if is_e2_emmet then
      return true
    end
  end
end

---@type LazySpec
return {
  'iguanacucumber/magazine.nvim',
  name = 'nvim-cmp',
  dependencies = {
    'lukas-reineke/cmp-rg',
    'kristijanhusak/vim-dadbod-completion',
    'https://codeberg.org/FelipeLema/cmp-async-path',
    { 'iguanacucumber/mag-nvim-lsp', name = 'cmp-nvim-lsp', opts = {} },
    { 'iguanacucumber/mag-nvim-lua', name = 'cmp-nvim-lua' },
    { 'iguanacucumber/mag-buffer', name = 'cmp-buffer' },
    { 'iguanacucumber/mag-cmdline', name = 'cmp-cmdline' },
    'saadparwaiz1/cmp_luasnip',
    {
      'L3MON4D3/LuaSnip',
      version = 'v2.*',
      enabled = true,
      build = 'make install_jsregexp',
      dependencies = {
        {
          'rafamadriz/friendly-snippets',
          config = function()
            require('luasnip.loaders.from_vscode').lazy_load()
            require('luasnip.loaders.from_vscode').lazy_load({ paths = { vim.fn.stdpath 'config' .. '/snippets' } })

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
      },
      opts = {
        history = true,
        delete_check_events = 'TextChanged',
      },
    },
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      dependencies = { 'Bilal2453/luvit-meta' },
      opts = {
        library = {
          'lazy.nvim',
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
        },
      },
    },
  },
  config = function()
    local cmp = require 'cmp'
    local types = require 'cmp.types'
    local defaults = require 'cmp.config.default'()
    local icon_provider = get_icon_provider()
    local function format(entry, item)
      local highlight_colors_avail, highlight_colors = pcall(require, 'nvim-highlight-colors')
      local color_item = highlight_colors_avail and highlight_colors.format(entry, { kind = item.kind })
      if icon_provider then
        local icon = icon_provider(item.kind)
        if icon then
          item.kind = icon
        end
      end
      if color_item and color_item.abbr and color_item.abbr_hl_group then
        item.kind, item.kind_hl_group = color_item.abbr, color_item.abbr_hl_group
      end
      return item
    end
    vim.opt.completeopt = 'menu,menuone,noinsert'
    cmp.setup({
      preselect = cmp.PreselectMode.None,
      completion = { completeopt = 'menu,menuone,noinsert' },
      sources = cmp.config.sources({
        { name = 'lazydev', priority = 1000, group_index = 0 },
        { name = 'nvim_lsp', priority = 1000 },
        { name = 'luasnip', priority = 750 },
        { name = 'async_path', priority = 600 },
        { name = 'buffer', priority = 300, group_index = 2 },
        {
          name = 'rg',
          keyword_length = 3,
          max_item_count = 10,
          group_index = 2,
          priority = 200,
          option = { additional_arguments = '--smart-case' },
        },
      }),
      confirmation = {
        default_behavior = cmp.SelectBehavior.Select,
      },
      sorting = {
        priority_weight = defaults.sorting.priority_weight,
        comparators = vim.tbl_extend('keep', {
          deprio(types.lsp.CompletionItemKind.Text),
          lower_emmet(),
        }, defaults.sorting.comparators),
      },
      window = {
        completion = cmp.config.window.bordered({
          col_offset = -2,
          side_padding = 0,
          border = 'rounded',
          winhighlight = 'Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
        }),
        documentation = cmp.config.window.bordered({
          border = 'rounded',
          winhighlight = 'Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
        }),
      },
      formatting = {
        fields = { 'kind', 'abbr', 'menu' },
        format = format,
      },
      mapping = {
        ['<Up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
        ['<Down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
        ['<C-P>'] = cmp.mapping(function()
          if is_visible(cmp) then
            cmp.select_prev_item()
          else
            cmp.complete()
          end
        end),
        ['<C-N>'] = cmp.mapping(function()
          if is_visible(cmp) then
            cmp.select_next_item()
          else
            cmp.complete()
          end
        end),
        ['<C-K>'] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), { 'i', 'c' }),
        ['<C-J>'] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), { 'i', 'c' }),
        ['<C-U>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-D>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        ['<C-Y>'] = cmp.config.disable,
        ['<C-E>'] = cmp.mapping(cmp.mapping.abort(), { 'i', 'c' }),
        ['<CR>'] = cmp.mapping({
          i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
          c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
        }),
        ['<C-x><C-x>'] = cmp.mapping.complete({
          config = { sources = { { name = 'luasnip' } } },
        }),
        ['<Tab>'] = cmp.mapping(function(fallback)
          if is_visible(cmp) then
            cmp.select_next_item()
          elseif vim.api.nvim_get_mode().mode ~= 'c' and vim.snippet and vim.snippet.active({ direction = 1 }) then
            vim.schedule(function()
              vim.snippet.jump(1)
            end)
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if is_visible(cmp) then
            cmp.select_prev_item()
          elseif vim.api.nvim_get_mode().mode ~= 'c' and vim.snippet and vim.snippet.active({ direction = -1 }) then
            vim.schedule(function()
              vim.snippet.jump(-1)
            end)
          else
            fallback()
          end
        end, { 'i', 's' }),
      },
    })
    cmp.setup.cmdline('/', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = 'buffer' },
      },
    })
    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = 'path' },
      }, {
        {
          name = 'cmdline',
          option = {
            ignore_cmds = { 'Man', '!', 'find', 'fin' },
          },
        },
      }),
    })
  end,
}
