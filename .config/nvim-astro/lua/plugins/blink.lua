---@type LazySpec
return {
  {
    "saghen/blink.cmp",
    optional = true,
    dependencies = { "mikavilpas/blink-ripgrep.nvim" },
    opts = {
      keymap = {
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<CR>"] = { "select_and_accept", "fallback" },
      },
      sources = {
        default = { "ripgrep" },
        providers = {
          -- Keep exact snippet triggers (rafce, rfc) above vtsls auto-imports.
          lsp = { score_offset = 20, fallbacks = {}, max_items = 50 },
          snippets = { score_offset = 30 },
          buffer = {
            score_offset = 2,
            opts = {
              get_bufnrs = function()
                return vim.tbl_filter(
                  function(buf) return vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == "" end,
                  vim.api.nvim_list_bufs()
                )
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
                ripgrep = {
                  search_casing = "--smart-case",
                  additional_rg_options = { "-g", "!node_modules/**" },
                },
              },
            },
          },
        },
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
      fuzzy = {
        implementation = "prefer_rust",
        sorts = {
          function(a, b)
            if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then return end
            return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
          end,
          "exact",
          "score",
          "sort_text",
        },
      },
    },
  },
  {
    "L3MON4D3/LuaSnip",
    -- Load on FileType so react-es7 snippets exist before the first InsertEnter.
    event = "FileType",
    config = function(plugin, opts)
      -- include the default astronvim config that calls the setup call
      require "astronvim.plugins.configs.luasnip"(plugin, opts)
      -- load snippets paths
      require("luasnip.loaders.from_vscode").lazy_load {
        -- paths = { vim.fn.expand "~/.config/nvim-astro/snippets/" },
        paths = { vim.fn.stdpath "config" .. "/snippets/" },
      }
    end,
  },
}
