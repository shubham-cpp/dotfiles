---@type LazySpec
return {
  {
    "nvim-cmp",
    keys = { ":", "/", "?" }, -- lazy load cmp on more keys along with insert mode
    dependencies = {
      -- { "hrsh7th/cmp-path", enabled = false },
      -- { "hrsh7th/cmp-nvim-lsp", enabled = false },
      -- { "hrsh7th/cmp-buffer", enabled = false },
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
      { "iguanacucumber/mag-nvim-lsp", name = "cmp-nvim-lsp", opts = {} },
      { "iguanacucumber/mag-buffer", name = "cmp-buffer" },
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
      "lukas-reineke/cmp-rg",
      "https://codeberg.org/FelipeLema/cmp-async-path",
    },
    opts = function(_, opts)
      local cmp = require "cmp"
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
      -- modify the sources part of the options table
      opts.sources = cmp.config.sources {
        { name = "nvim_lsp", priority = 1000 },
        { name = "luasnip", priority = 750 },
        { name = "buffer", priority = 500 },
        { name = "async_path", priority = 600 },
        {
          name = "rg",
          keyword_length = 3,
          max_item_count = 10,
          priority = 200,
        },
      }
    end,
    config = function(_, opts)
      local cmp = require "cmp"
      -- run cmp setup
      cmp.setup(opts)

      -- configure `cmp-cmdline` as described in their repo: https://github.com/hrsh7th/cmp-cmdline#setup
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
    config = function(...)
      require "astronvim.plugins.configs.luasnip"(...)
      require("luasnip.loaders.from_vscode").lazy_load { paths = { vim.fn.stdpath "config" .. "/snippets" } }
    end,
  },
}
