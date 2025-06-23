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
        -- ["<C-space>"] = {
        --   -- 'show'
        --   function(cmp)
        --     cmp.show({ providers = { "snippets" } })
        --   end,
        --   "show_documentation",
        --   "hide_documentation",
        -- },
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
        sorts = {
          -- function(a, b)
          --   if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then
          --     return
          --   end
          --   return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
          -- end,
          "exact",
          -- default sorts
          "score",
          "sort_text",
        },
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
      sources = {
        default = { "ripgrep" },
        providers = {
          snippets = { score_offset = 100 },
          lsp = { score_offset = 100 },
          path = { score_offset = 120 },
          buffer = { score_offset = 40 },
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            ---@module "blink-ripgrep"
            ---@type blink-ripgrep.Options
            opts = {
              prefix_min_len = 4,
              score_offset = 20,
              max_filesize = "300K",
              search_casing = "--smart-case",
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
}
