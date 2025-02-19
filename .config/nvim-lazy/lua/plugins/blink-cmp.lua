local function has_words_before()
  local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
---@type LazySpec
return {
  {
    "saghen/blink.cmp",
    optional = true,
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
          function(cmp)
            if has_words_before() or vim.api.nvim_get_mode().mode == "c" then
              return cmp.show()
            end
          end,
          "fallback",
        },
        ["<S-Tab>"] = {
          "select_prev",
          "snippet_backward",
          function(cmp)
            if vim.api.nvim_get_mode().mode == "c" then
              return cmp.show()
            end
          end,
          "fallback",
        },
      },
      cmdline = {
        enabled = true,
        keymap = {
          preset = "enter",
          ["<Up>"] = {},
          ["<Down>"] = {},
          ["<C-k>"] = { "select_prev", "fallback" },
          ["<C-j>"] = { "select_next", "fallback" },
          ["<Tab>"] = { "select_next", "fallback" },
          ["<S-Tab>"] = { "select_prev", "fallback" },
        },
        sources = function()
          local type = vim.fn.getcmdtype()
          -- Search forward and backward
          if type == "/" or type == "?" then
            return { "buffer" }
          end
          -- Commands
          if type == ":" or type == "@" then
            return { "path", "cmdline" }
          end
          return {}
        end,
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
}
