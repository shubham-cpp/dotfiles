---@type LazySpec
return {
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    config = function(...)
      require "astronvim.plugins.configs.luasnip"(...)
      require("luasnip.loaders.from_vscode").lazy_load { paths = { vim.fn.stdpath "config" .. "/snippets" } }
    end,
  },
  {
    "Saghen/blink.cmp",
    optional = true,
    dependencies = { "mikavilpas/blink-ripgrep.nvim" },
    opts = {
      keymap = {
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<CR>"] = { "select_and_accept", "fallback" },
      },
      cmdline = {
        enabled = true,
        keymap = {
          preset = "cmdline",
          ["<Left>"] = {},
          ["<Right>"] = {},
          ["<C-j>"] = { "select_next", "fallback" },
          ["<C-k>"] = { "select_prev", "fallback" },
        },
        completion = {
          list = { selection = { preselect = false } },
          menu = { auto_show = true },
        },
      },
      sources = {
        default = { "ripgrep" },
        providers = {
          cmdline = {
            -- ignores cmdline completions when executing shell commands
            enabled = function() return vim.fn.getcmdtype() ~= ":" or not vim.fn.getcmdline():match "^[%%0-9,'<>%-]*!" end,
          },
          buffer = {
            opts = {
              get_bufnrs = function()
                return vim.tbl_filter(function(bufnr) return vim.bo[bufnr].buftype == "" end, vim.api.nvim_list_bufs())
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
      fuzzy = {
        implementation = "prefer_rust",
        -- sorts = {
        --   function(a, b)
        --     if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then return end
        --     return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
        --   end,
        --   -- default sorts
        --   "score",
        --   "sort_text",
        --   "exact",
        -- },
      },
    },
  },
}
