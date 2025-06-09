---@type LazySpec
return {
  {
    "Saghen/blink.cmp",
    optional = true,
    dependencies = "mikavilpas/blink-ripgrep.nvim",
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
              max_filesize = "300K",
              search_casing = "--smart-case",
            },
          },
        },
      },
      keymap = {
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<CR>"] = { "select_and_accept", "fallback" },
      },
      fuzzy = {
        sorts = {
          function(a, b)
            if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then return end
            return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
          end,
          "exact",
          -- default sorts
          "score",
          "sort_text",
        },
      },
      cmdline = {
        enabled = true,
        keymap = {
          preset = "cmdline",
          ["<C-j>"] = { "select_next", "fallback" },
          ["<C-k>"] = { "select_prev", "fallback" },
        },
        completion = {
          list = { selection = { preselect = false } },
          menu = {
            auto_show = function() return vim.fn.getcmdtype() == ":" or vim.fn.getcmdtype() == "@" end,
          },
        },
        sources = function()
          local type = vim.fn.getcmdtype()
          -- Search forward and backward
          if type == "/" or type == "?" then return { "buffer" } end
          -- Commands
          if type == ":" or type == "@" then return { "cmdline" } end
          return {}
        end,
      },
    },
  },
}
