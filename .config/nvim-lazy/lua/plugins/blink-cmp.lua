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
          function(a, b)
            if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then
              return
            end
            return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
          end,
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
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            ---@module "blink-ripgrep"
            ---@type blink-ripgrep.Options
            opts = {
              prefix_min_len = 4,
              score_offset = -3, -- should be lower priority
              max_filesize = "300K",
              search_casing = "--smart-case",
            },
          },
        },
      },
      -- fuzzy = {
      --   sorts = {
      --     function(a, b)
      --       if a.client_name == nil or b.client_name == nil then
      --         return nil
      --       end
      --       return b.client_name == "emmet_ls"
      --     end,
      --     "exact",
      --     "score",
      --     "sort_text",
      --   },
      -- },
    },
  },
  {
    "L3MON4D3/LuaSnip",
    opts = {
      history = true,
      delete_check_events = "TextChanged",
      region_check_events = "CursorMoved",
    },
  },
}
