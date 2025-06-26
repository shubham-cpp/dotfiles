---@type LazySpec
return {
  {
    "L3MON4D3/LuaSnip",
    version = "v2.*",
    build = "make install_jsregexp",
    dependencies = {
      {
        "rafamadriz/friendly-snippets",
        config = function()
          require("luasnip.loaders.from_vscode").lazy_load()
          require("luasnip.loaders.from_vscode").lazy_load { paths = { vim.fn.stdpath "config" .. "/snippets" } }
        end,
      },
    },
    opts = {
      history = true,
      delete_check_events = "TextChanged",
    },
  },
  {
    "saghen/blink.cmp",
    enabled = false,
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = { "L3MON4D3/LuaSnip", "mikavilpas/blink-ripgrep.nvim", "windwp/nvim-autopairs" },
    version = "1.*",
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      sources = {
        -- add lazydev to your completion providers
        default = { "lazydev", "lsp", "path", "snippets", "buffer", "ripgrep" },
        providers = {
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
            score_offset = 12,
          },
          lsp = { score_offset = 10, module = "blink.cmp.sources.lsp", name = "LSP" },
          path = { score_offset = 15, module = "blink.cmp.sources.path" },
          snippets = { score_offset = 9, module = "blink.cmp.sources.snippets" },
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            ---@module "blink-ripgrep"
            ---@type blink-ripgrep.Options
            opts = {
              prefix_min_len = 4,
              score_offset = -10,
              max_filesize = "300K",
              search_casing = "--smart-case",
            },
          },
        },
      },
      keymap = {
        preset = "enter",
        ["<C-s>"] = {
          function(cmp)
            cmp.show { providers = { "snippets" } }
          end,
        },
        ["<C-h>"] = { "show_signature", "hide_signature", "fallback" },
        ["<C-k>"] = { "select_prev", "fallback" },
        ["<C-j>"] = { "select_next", "fallback" },
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
      },
      cmdline = {
        enabled = true,
        sources = function()
          local type = vim.fn.getcmdtype()
          -- Search forward and backward
          if type == "/" or type == "?" then
            return { "buffer" }
          end
          -- Commands
          if type == ":" or type == "@" then
            return { "cmdline" }
          end
          return {}
        end,
        keymap = {
          preset = "cmdline",
          -- ["<Up>"] = {},
          -- ["<Down>"] = {},
          ["<Left>"] = {},
          ["<Right>"] = {},
          ["<C-k>"] = { "select_prev", "fallback" },
          ["<C-j>"] = { "select_next", "fallback" },
          ["<Tab>"] = { "select_next", "fallback" },
          ["<S-Tab>"] = { "select_prev", "fallback" },
        },
        completion = {
          list = { selection = { preselect = false } },
          menu = {
            auto_show = function()
              local type = vim.fn.getcmdtype()
              return (type == ":" and not type:match "^[%%0-9,'<>%-]*!") or type == "@" or type == "?" or type == "/"
            end,
          },
        },
      },
      completion = {
        accept = { auto_brackets = { enabled = true } },
        ghost_text = { enabled = false },
        documentation = { auto_show = true, auto_show_delay_ms = 200 },
        list = {
          selection = {
            preselect = function(ctx)
              return ctx.mode ~= "cmdline"
            end,
          },
        },
        menu = {
          draw = {
            components = {
              kind_icon = {
                text = function(ctx)
                  local kind_icon, _, _ = require("mini.icons").get("lsp", ctx.kind)
                  return kind_icon
                end,
                -- (optional) use highlights from mini.icons
                highlight = function(ctx)
                  local _, hl, _ = require("mini.icons").get("lsp", ctx.kind)
                  return hl
                end,
              },
              kind = {
                -- (optional) use highlights from mini.icons
                highlight = function(ctx)
                  local _, hl, _ = require("mini.icons").get("lsp", ctx.kind)
                  return hl
                end,
              },
            },
          },
        },
      },
      -- fuzzy = {
      --   sorts = {
      --     -- function(a, b)
      --     --   if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then
      --     --     return
      --     --   end
      --     --   return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
      --     -- end,
      --     "exact",
      --     -- default sorts
      --     "score",
      --     "sort_text",
      --   },
      -- },
      snippets = { preset = "luasnip" },
      signature = { enabled = true },
    },
    opts_extend = { "sources.default" },
  },
}
