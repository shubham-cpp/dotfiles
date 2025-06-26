---@type LazySpec
return {
  "hrsh7th/nvim-cmp",
  enabled = true,
  event = { "InsertEnter", "CmdlineEnter" },
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "https://codeberg.org/FelipeLema/cmp-async-path",
    "hrsh7th/cmp-cmdline",
    "L3MON4D3/LuaSnip",
    "saadparwaiz1/cmp_luasnip",
    "lukas-reineke/cmp-rg",
  },
  opts = function(_, opts)
    opts = opts or {}

    local cmp = require "cmp"
    local ok_luasnip, luasnip = pcall(require, "luasnip")

    local defaults = require "cmp.config.default"()

    local function has_words_before()
      local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
      return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match "%s" == nil
    end
    local function is_visible(cmp)
      return cmp.core.view:visible() or vim.fn.pumvisible() == 1
    end

    local function get_icon_provider()
      local _, mini_icons = pcall(require, "mini.icons")
      if _G.MiniIcons then
        return function(kind)
          return mini_icons.get("lsp", kind or "")
        end
      end
      local lspkind_avail, lspkind = pcall(require, "lspkind")
      if lspkind_avail then
        return function(kind)
          return lspkind.symbolic(kind, { mode = "symbol" })
        end
      end
    end
    local icon_provider = get_icon_provider()

    local function format(entry, item)
      local highlight_colors_avail, highlight_colors = pcall(require, "nvim-highlight-colors")
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

    opts.snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    }
    opts.formatting = { fields = { "kind", "abbr", "menu" }, format = format }
    opts.mapping = cmp.mapping.preset.insert {
      ["<Up>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
      ["<Down>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
      ["<C-P>"] = cmp.mapping(function()
        if is_visible(cmp) then
          cmp.select_prev_item()
        else
          cmp.complete()
        end
      end),
      ["<C-N>"] = cmp.mapping(function()
        if is_visible(cmp) then
          cmp.select_next_item()
        else
          cmp.complete()
        end
      end),
      ["<C-x><C-x>"] = cmp.mapping.complete {
        config = { sources = { { name = "luasnip" } } },
      },
      ["<C-K>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
      ["<C-J>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
      ["<C-U>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
      ["<C-D>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
      ["<C-E>"] = cmp.mapping(cmp.mapping.abort(), { "i", "c" }),
      ["<CR>"] = { i = cmp.mapping.confirm { select = true }, c = cmp.mapping.confirm { select = false } },
      ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
      ["<Tab>"] = cmp.mapping(function(fallback)
        if is_visible(cmp) then
          cmp.select_next_item()
        elseif vim.api.nvim_get_mode().mode ~= "c" and ok_luasnip and luasnip.expand_or_locally_jumpable() then
          luasnip.expand_or_jump()
        elseif vim.api.nvim_get_mode().mode ~= "c" and vim.snippet and vim.snippet.active { direction = 1 } then
          vim.schedule(function()
            vim.snippet.jump(1)
          end)
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if is_visible(cmp) then
          cmp.select_prev_item()
        elseif vim.api.nvim_get_mode().mode ~= "c" and ok_luasnip and luasnip.jumpable(-1) then
          luasnip.jump(-1)
        elseif vim.api.nvim_get_mode().mode ~= "c" and vim.snippet and vim.snippet.active { direction = -1 } then
          vim.schedule(function()
            vim.snippet.jump(-1)
          end)
        else
          fallback()
        end
      end, { "i", "s" }),
    }

    opts.sources = cmp.config.sources {
      { name = "lazydev", priority = 1100, group_index = 0 },
      { name = "nvim_lsp", priority = 1000, group_index = 1 },
      { name = "luasnip", priority = 900, group_index = 1 },
      { name = "async_path", priority = 1200, group_index = 1 },
      { name = "buffer", priority = 300, group_index = 2 },
      {
        name = "rg",
        keyword_length = 3,
        max_item_count = 10,
        group_index = 2,
        priority = 200,
        option = { additional_arguments = "--smart-case" },
      },
    }
    opts.sorting = defaults.sorting
  end,
  config = function(_, opts)
    local cmp = require "cmp"

    cmp.setup(opts)

    cmp.setup.cmdline({ "/", "?" }, {
      mapping = cmp.mapping.preset.cmdline(),
      sources = { { name = "buffer" } },
    })
    cmp.setup.cmdline(":", {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = "path" },
      }, {
        {
          name = "cmdline",
          option = {
            ignore_cmds = { "Man", "!" },
          },
        },
      }),
      matching = { disallow_symbol_nonprefix_matching = false },
    })
  end,
}
