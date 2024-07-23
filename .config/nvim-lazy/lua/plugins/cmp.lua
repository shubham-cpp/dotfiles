local function feedkey(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end
-- local function has_words_before()
--   unpack = unpack or table.unpack
--   local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--   return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
-- end
---@type LazySpec
return {
  "hrsh7th/nvim-cmp",
  opts = function(_, opts)
    local cmp = require("cmp")
    opts.mapping = vim.tbl_extend("force", opts.mapping, {
      ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
      ["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
      ["<C-n>"] = cmp.mapping({
        c = function()
          if cmp.visible() then
            cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
          else
            feedkey("<Down>", "n")
          end
        end,
        i = function(fallback)
          if cmp.visible() then
            cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
          else
            fallback()
          end
        end,
      }),
      ["<C-p>"] = cmp.mapping({
        c = function()
          if cmp.visible() then
            cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
          else
            feedkey("<Up>", "n")
          end
        end,
        i = function(fallback)
          if cmp.visible() then
            cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
          else
            fallback()
          end
        end,
      }),
      ["<C-x><C-x>"] = cmp.mapping.complete({
        config = { sources = { { name = "luasnip" } } },
      }),
      ["<C-x><C-f>"] = cmp.mapping.complete({
        config = { sources = { { name = "path" } } },
      }),
    })
    opts.sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "path" },
      { name = "luasnip" },
    }, {
      {
        name = "buffer",
        option = {
          keyword_length = 3,
          get_bufnrs = function()
            local bufIsSmall = function(bufnr)
              return vim.api.nvim_buf_line_count(bufnr) < 2000
            end
            return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
            -- return vim.api.nvim_list_bufs()
          end,
        },
      },
    })
    opts.experimental = {
      ghost_text = false,
    }
  end,
}
