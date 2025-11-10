---@type LazySpec
return {
  {
    "saghen/blink.cmp",
    optional = true,
    events = { "InsertEnter", "CmdlineEnter" },
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      keymap = {
        preset = "enter",
        ["<C-s>"] = {
          function(cmp)
            cmp.show({ providers = { "snippets" } })
          end,
        },
        ["<C-h>"] = { "show_signature", "hide_signature", "fallback" },
        ["<C-k>"] = { "select_prev", "fallback" },
        ["<C-j>"] = { "select_next", "fallback" },
        ["<Tab>"] = {
          "select_next",
          "snippet_forward",
          "fallback",
        },
        ["<S-Tab>"] = {
          "select_prev",
          "snippet_backward",
          "fallback",
        },
      },
      cmdline = {
        keymap = {
          preset = "cmdline",
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
              return (type == ":" and not type:match("^[%%0-9,'<>%-]*!")) or type == "@" or type == "?" or type == "/"
            end,
          },
        },
      },
      completion = {
        ghost_text = { enabled = false },
        list = {
          selection = {
            preselect = function(ctx)
              return ctx.mode ~= "cmdline"
            end,
          },
        },
      },
      fuzzy = {
        implementation = "prefer_rust",
        -- sorts = {
        --   function(a, b)
        --     if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then
        --       return
        --     end
        --     return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
        --   end,
        --   "score",
        --   "sort_text",
        --   "exact",
        -- },
      },
    },
  },
  {
    "saghen/blink.cmp",
    optional = true,
    dependencies = { "mikavilpas/blink-ripgrep.nvim" },
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      -- snippets = { score_offset = 9 },
      sources = {
        default = { "ripgrep" },
        providers = {
          -- snippets = { score_offset = 9 },
          -- lsp = { score_offset = 10 },
          -- path = { score_offset = 30 },
          buffer = {
            score_offset = -3,
            opts = {
              get_bufnrs = function()
                return vim.tbl_filter(function(bufnr)
                  return vim.bo[bufnr].buftype == ""
                end, vim.api.nvim_list_bufs())
              end,
            },
          },
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            score_offset = -5,
            ---@module "blink-ripgrep"
            ---@type blink-ripgrep.Options
            opts = {
              prefix_min_len = 4,
              backend = {
                ripgrep = { search_casing = "--smart-case" },
              },
            },
          },
        },
      },
    },
  },
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    opts = {
      history = true,
      delete_check_events = "TextChanged",
      region_check_events = "CursorMoved",
    },
  },
  {
    "rafamadriz/friendly-snippets",
    optional = true,
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
      require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.expand("~/.config/nvim/snippets") } })
    end,
  },
}
