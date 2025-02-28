---@param kind lsp.CompletionItemKind | number
---@return cmp.ComparatorFunction
local function deprio(kind)
  return function(e1, e2)
    if e1:get_kind() == kind then return false end
    if e2:get_kind() == kind then return true end
  end
end

---@return cmp.ComparatorFunction
local function lower_emmet()
  return function(e1, e2)
    local is_e1_emmet = e1.source:get_debug_name() == "nvim_lsp:emmet_ls"
      or e1.source:get_debug_name() == "nvim_lsp:emmet_language_server"
    local is_e2_emmet = e2.source:get_debug_name() == "nvim_lsp:emmet_ls"
      or e2.source:get_debug_name() == "nvim_lsp:emmet_language_server"
    if is_e1_emmet then return false end
    if is_e2_emmet then return true end
    return nil
  end
end

---@type LazySpec
return {
  -- {
  --   "lspkind.nvim",
  --   opts = {
  --     mode = "text_symbol",
  --   },
  -- },
  {
    "nvim-cmp",
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
      local cmp = require "cmp"
      local types = require "cmp.types"
      local defaults = require "cmp.config.default"()
      opts.mapping["<C-x><C-x>"] = cmp.mapping.complete {
        config = { sources = { { name = "luasnip" } } },
      }
      opts.mapping["<C-y>"] = cmp.mapping {
        i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
        c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
      }
      opts.mapping["<CR>"] = cmp.mapping {
        i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
        c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
      }

      opts.sources = cmp.config.sources {
        { name = "lazydev", priority = 1000, group_index = 0 },
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
      }

      opts.sorting = {
        priority_weight = defaults.sorting.priority_weight,
        comparators = vim.tbl_extend("keep", {
          deprio(types.lsp.CompletionItemKind.Text),
          lower_emmet(),
        }, defaults.sorting.comparators),
      }
    end,
    config = function(_, opts)
      local cmp = require "cmp"
      cmp.setup(opts)

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
    "LuaSnip",
    optional = true,
    config = function(...)
      require "astronvim.plugins.configs.luasnip"(...)
      require("luasnip.loaders.from_vscode").lazy_load { paths = { vim.fn.stdpath "config" .. "/snippets" } }
    end,
  },
  {
    "blink.cmp",
    optional = true,
    dependencies = { "mikavilpas/blink-ripgrep.nvim" },
    opts = {
      sources = {
        default = { "ripgrep" },
        providers = {
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            ---@module "blink-ripgrep"
            ---@type blink-ripgrep.Options
            opts = {},
          },
        },
      },
    },
  },
}
