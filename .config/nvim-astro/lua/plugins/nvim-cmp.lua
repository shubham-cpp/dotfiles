--- detect if the current completion item is an emmet completion item
--- @param entry cmp.Entry
--- @return boolean
local function isEmmet(entry)
  return (
    entry:get_kind() == require("cmp.types").lsp.CompletionItemKind.Text
    or entry:get_kind() == require("cmp.types").lsp.CompletionItemKind.Snippet
  )
    and (
      entry.source:get_debug_name() == "nvim_lsp:emmet_language_server"
      or entry.source:get_debug_name() == "nvim_lsp:emmet_ls"
    )
end
---@type LazySpec
return {
  {
    "iguanacucumber/magazine.nvim",
    lazy = true,
    config = function()
      vim.opt.rtp:remove(require("astrocore").get_plugin("nvim-cmp").dir)
      vim.opt.rtp:remove(require("astrocore").get_plugin("cmp-buffer").dir)
      vim.opt.rtp:remove(require("astrocore").get_plugin("cmp-nvim-lsp").dir)
    end,
    specs = {
      "hrsh7th/nvim-cmp",
      dependencies = {
        "iguanacucumber/magazine.nvim",
        { "hrsh7th/cmp-path", enabled = false },
        { "hrsh7th/cmp-buffer", enabled = true },
        { "hrsh7th/cmp-nvim-lsp", enabled = true },
      },
      opts = function(_, opts)
        local cmp = require "cmp"

        opts.mapping["<C-x><C-x>"] = cmp.mapping.complete {
          config = { sources = { { name = "luasnip" } } },
        }
        opts.mapping["<C-x><C-e>"] = cmp.mapping.complete {
          config = {
            sources = {
              {
                name = "nvim_lsp",
                entry_filter = function(entry) return isEmmet(entry) end,
              },
            },
          },
        }
        opts.mapping["<C-y>"] = cmp.mapping {
          i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
          c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
        }
        opts.mapping["<CR>"] = cmp.mapping {
          i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
          c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
        }
      end,
      config = function(_, opts)
        local cmp, astro = require "cmp", require "astrocore"
        local sources = {}
        for source_plugin, source in pairs {
          ["lazydev.nvim"] = {
            name = "lazydev",
            group_index = 0, -- set group index to 0 to skip loading LuaLS completions
          },
          ["mag-buffer"] = { name = "buffer", priority = 500, group_index = 2 },
          ["cmp_luasnip"] = { name = "luasnip", priority = 750 },
          ["mag-nvim-lsp"] = {
            name = "nvim_lsp",
            priority = 1000,
            entry_filter = function(entry) return not isEmmet(entry) end,
          },
          ["cmp-path"] = { name = "path", priority = 250 },
          ["cmp-async-path"] = { name = "async_path", priority = 250 },
          ["cmp-rg"] = { name = "rg", priority = 350, group_index = 2 },
        } do
          if astro.is_available(source_plugin) then table.insert(sources, source) end
        end
        opts.sources = sources
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
                ignore_cmds = { "Man", "!" },
              },
            },
          }),
        })
      end,
    },
    keys = { ":", "/", "?" },
    dependencies = {
      { "iguanacucumber/mag-nvim-lsp", opts = {} },
      { "iguanacucumber/mag-buffer" },
      { "iguanacucumber/mag-cmdline", name = "cmp-cmdline", lazy = true },
      "lukas-reineke/cmp-rg",
      "https://codeberg.org/FelipeLema/cmp-async-path",
    },
  },
}
