local function has_words_before()
  local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
local function is_visible(cmp)
  return cmp.core.view:visible() or vim.fn.pumvisible() == 1
end

---@param kind lsp.CompletionItemKind | number
---@return cmp.ComparatorFunction
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

---@return cmp.ComparatorFunction
local function lower_emmet()
  return function(e1, e2)
    local is_e1_emmet = e1.source:get_debug_name() == "nvim_lsp:emmet_ls"
      or e1.source:get_debug_name() == "nvim_lsp:emmet_language_server"
    local is_e2_emmet = e2.source:get_debug_name() == "nvim_lsp:emmet_ls"
      or e2.source:get_debug_name() == "nvim_lsp:emmet_language_server"
    if is_e1_emmet then
      return false
    end
    if is_e2_emmet then
      return true
    end
    return nil
  end
end
---@type LazySpec
return {
  {
    "iguanacucumber/magazine.nvim",
    name = "nvim-cmp",
    optional = true,
    keys = { ":", "/", "?" }, -- lazy load cmp on more keys along with insert mode
    dependencies = {
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
      { "iguanacucumber/mag-nvim-lsp", name = "cmp-nvim-lsp", opts = {} },
      { "iguanacucumber/mag-buffer", name = "cmp-buffer" },
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
      "https://codeberg.org/FelipeLema/cmp-async-path",
      "lukas-reineke/cmp-rg",
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      local types = require("cmp.types")
      local defaults = require("cmp.config.default")()

      opts.mapping["<C-x><C-x>"] = cmp.mapping.complete({
        config = { sources = { { name = "luasnip" } } },
      })
      opts.mapping["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select })
      opts.mapping["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select })
      opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
        if is_visible(cmp) then
          cmp.select_next_item()
        elseif vim.api.nvim_get_mode().mode ~= "c" and vim.snippet and vim.snippet.active({ direction = 1 }) then
          vim.schedule(function()
            vim.snippet.jump(1)
          end)
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end)
      opts.mapping["<S-Tab>"] = cmp.mapping(function(fallback)
        if is_visible(cmp) then
          cmp.select_prev_item()
        elseif vim.api.nvim_get_mode().mode ~= "c" and vim.snippet and vim.snippet.active({ direction = -1 }) then
          vim.schedule(function()
            vim.snippet.jump(-1)
          end)
        else
          fallback()
        end
      end, { "i", "s" })

      opts.experimental = { ghost_text = false }

      opts.sources = cmp.config.sources({
        { name = "lazydev", group_index = 0 },
        { name = "nvim_lsp", priority = 1000, group_index = 1 },
        { name = "luasnip", priority = 750, group_index = 1 },
        { name = "async_path", priority = 600, group_index = 2 },
        { name = "buffer", priority = 300, group_index = 2 },
        {
          name = "rg",
          keyword_length = 3,
          max_item_count = 10,
          group_index = 2,
          priority = 200,
          option = { additional_arguments = "--smart-case" },
        },
      })
      opts.sorting = {
        priority_weight = defaults.sorting.priority_weight,
        comparators = vim.tbl_extend("keep", {
          deprio(types.lsp.CompletionItemKind.Text),
          lower_emmet(),
        }, defaults.sorting.comparators),
      }

      cmp.setup.cmdline("/", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
      })
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          {
            name = "cmdline",
            option = {
              ignore_cmds = { "Man", "!", "find", "fin" },
            },
          },
        }),
      })
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    lazy = true,
    specs = {
      {
        "nvim-cmp",
        optional = true,
        dependencies = { { "saadparwaiz1/cmp_luasnip", lazy = true } },
        opts = function(_, opts)
          local luasnip, cmp = require("luasnip"), require("cmp")

          if not opts.snippet then
            opts.snippet = {}
          end
          opts.snippet.expand = function(args)
            luasnip.lsp_expand(args.body)
          end

          if not opts.mappings then
            opts.mappings = {}
          end
          opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
            if is_visible(cmp) then
              cmp.select_next_item()
            elseif vim.api.nvim_get_mode().mode ~= "c" and luasnip.expand_or_locally_jumpable() then
              luasnip.expand_or_jump()
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end, { "i", "s" })
          opts.mapping["<S-Tab>"] = cmp.mapping(function(fallback)
            if is_visible(cmp) then
              cmp.select_prev_item()
            elseif vim.api.nvim_get_mode().mode ~= "c" and luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" })
        end,
      },
    },
  },
}
