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
  dependencies = {
    { "hrsh7th/cmp-path", enabled = false },
    "lukas-reineke/cmp-rg",
    "octaltree/cmp-look",
    "https://codeberg.org/FelipeLema/cmp-async-path",
  },
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
      ["<C-x><C-e>"] = cmp.mapping.complete({
        config = {
          sources = {
            {
              name = "nvim_lsp",
              entry_filter = function(entry)
                vim.print(entry.source:get_debug_name())
                if
                  (
                    entry:get_kind() == require("cmp.types").lsp.CompletionItemKind.Text
                    or entry:get_kind() == require("cmp.types").lsp.CompletionItemKind.Snippet
                  ) and entry.source:get_debug_name() == "nvim_lsp:emmet_language_server"
                then
                  return true
                end
                return false
              end,
            },
          },
        },
      }),
      ["<C-x><C-f>"] = cmp.mapping.complete({
        config = { sources = { { name = "path" } } },
      }),
      ["<C-y>"] = cmp.mapping({
        i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
        c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
      }),
    })
    opts.sources = cmp.config.sources({
      {
        name = "nvim_lsp",
        max_item_count = 20,
        entry_filter = function(entry)
          if
            (
              entry:get_kind() == require("cmp.types").lsp.CompletionItemKind.Text
              or entry:get_kind() == require("cmp.types").lsp.CompletionItemKind.Snippet
            ) and entry.source:get_debug_name() == "nvim_lsp:emmet_language_server"
          then
            return false
          end
          return true
        end,
      },
      -- { name = "path" },
      { name = "luasnip" },
      { name = "async_path", option = { trailing_slash = false } },
    }, {
      {
        name = "buffer",
        option = {
          keyword_length = 2,
          -- get_bufnrs = function()
          --   local bufIsSmall = function(bufnr)
          --     return vim.api.nvim_buf_line_count(bufnr) < 2000
          --   end
          --   return vim.tbl_filter(bufIsSmall, vim.api.nvim_list_bufs())
          --   -- return vim.api.nvim_list_bufs()
          -- end,
        },
      },
      {
        name = "rg",
        keyword_length = 3,
        max_item_count = 10,
        priority_weight = 50,
        option = {
          additional_arguments = "--smart-case --hidden",
          set_filetype = true,
          marker = " ❰❰❰",
        },
        -- entry_filter = function(entry)
        --   return not entry.exact
        -- end,
      },
      {
        name = "look",
        keyword_length = 4,
        priority = 30,
        option = {
          convert_case = true,
          loud = true,
          --dict = '/usr/share/dict/words'
        },
      },
    })
    opts.experimental = {
      ghost_text = false,
    }
  end,
}
