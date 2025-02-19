---@param kind lsp.CompletionItemKind | number
---@return cmp.ComparatorFunction
local function deprio(kind)
  return function(e1, e2)
    if e1:get_kind() == kind then return false end
    if e2:get_kind() == kind then return true end
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
      -- { "hrsh7th/cmp-path", enabled = false },
      -- { "hrsh7th/cmp-nvim-lsp", enabled = false },
      -- { "hrsh7th/cmp-buffer", enabled = false },
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
      { "iguanacucumber/mag-nvim-lsp", name = "cmp-nvim-lsp", opts = {} },
      { "iguanacucumber/mag-buffer", name = "cmp-buffer" },
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
      "https://codeberg.org/FelipeLema/cmp-async-path",
      "lukas-reineke/cmp-rg",
      {
        "milanglacier/minuet-ai.nvim",
        enabled = false,
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
          provider = "openai_fim_compatible",
          n_completions = 1, -- recommend for local model for resource saving
          -- I recommend beginning with a small context window size and incrementally
          -- expanding it, depending on your local computing power. A context window
          -- of 512, serves as an good starting point to estimate your computing
          -- power. Once you have a reliable estimate of your local computing power,
          -- you should adjust the context window to a larger value.
          context_window = 512,
          provider_options = {
            openai_fim_compatible = {
              api_key = "TERM",
              name = "Ollama",
              end_point = "http://localhost:11434/v1/completions",
              model = "starcoder2:latest",
              optional = {
                max_tokens = 256,
                top_p = 0.9,
              },
            },
          },
        },
        config = function(_, opts) require("minuet").setup(opts) end,
      },
    },
    opts = function(_, opts)
      local cmp = require "cmp"
      local types = require "cmp.types"
      local defaults = require "cmp.config.default"()
      opts.mapping["<C-x><C-x>"] = cmp.mapping.complete {
        config = { sources = { { name = "luasnip" } } },
      }
      -- opts.completion = { completeopt = "menu,menuone,preview" }
      opts.mapping["<C-y>"] = cmp.mapping {
        i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
        c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
      }
      opts.mapping["<CR>"] = cmp.mapping {
        i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
        c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
      }
      -- opts.mapping["<A-y>"] = require("minuet").make_cmp_map()
      -- modify the sources part of the options table
      opts.sources = cmp.config.sources {
        { name = "nvim_lsp", priority = 1000 },
        { name = "luasnip", priority = 750 },
        { name = "buffer", priority = 300 },
        { name = "async_path", priority = 600 },
        {
          name = "rg",
          keyword_length = 3,
          max_item_count = 10,
          priority = 200,
          option = { additional_arguments = "--smart-case" },
        },
        -- { name = "minuet" },
      }
      -- opts.performance = require("astrocore").extend_tbl(opts.performance or {}, {
      --   -- It is recommended to increase the timeout duration due to
      --   -- the typically slower response speed of LLMs compared to
      --   -- other completion sources. This is not needed when you only
      --   -- need manual completion.
      --   fetching_timeout = 2000,
      -- })

      opts.sorting = {
        priority_weight = defaults.sorting.priority_weight,
        comparators = vim.tbl_extend("keep", {
          deprio(types.lsp.CompletionItemKind.Text),
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
