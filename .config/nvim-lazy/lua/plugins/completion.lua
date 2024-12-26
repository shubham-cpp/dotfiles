local function feedkey(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end

local function deprio(kind)
  return function(e1, e2)
    if e1:get_kind() == kind then
      return false
    end
    if e2:get_kind() == kind then
      return true
    end
  end
end

---@type LazySpec
return {
  {
    "saghen/blink.cmp",
    enabled = vim.g.lazyvim_cmp == "blink.cmp" or vim.g.lazyvim_cmp == "auto",
    dependencies = {
      "mikavilpas/blink-ripgrep.nvim",
    },
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      keymap = {
        preset = "enter",
        ["<C-y>"] = { "select_and_accept", "fallback" },
        ["<C-k>"] = { "select_prev", "fallback" },
        ["<C-j>"] = { "select_next", "fallback" },
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
        cmdline = {
          preset = "enter",
          ["<Up>"] = {},
          ["<Down>"] = {},
          ["<C-k>"] = { "select_prev", "fallback" },
          ["<C-j>"] = { "select_next", "fallback" },
          ["<Tab>"] = { "select_next", "fallback" },
          ["<S-Tab>"] = { "select_prev", "fallback" },
        },
      },
      completion = {
        menu = { border = "rounded" },
        documentation = { window = { border = "rounded" } },
        list = {
          selection = function(ctx)
            return ctx.mode == "cmdline" and "auto_insert" or "preselect"
          end,
        },
      },
      sources = {
        default = {
          "lsp",
          "path",
          "snippets",
          "buffer",
          "ripgrep",
        },
        providers = {
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            ---@module "blink-ripgrep"
            ---@type blink-ripgrep.Options
            opts = {
              prefix_min_len = 4,
              score_offset = 1, -- should be lower priority
              max_filesize = "300K",
              search_casing = "--smart-case",
            },
          },
        },
        cmdline = function()
          local type = vim.fn.getcmdtype()
          if type == ":" then
            return { "path", "cmdline" }
          end
          if type == "/" or type == "?" then
            return { "buffer" }
          end
          return {}
        end,
      },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    enabled = vim.g.lazyvim_cmp == "nvim-cmp",
    dependencies = {
      { "hrsh7th/cmp-path", enabled = false },
      "lukas-reineke/cmp-rg",
      "https://codeberg.org/FelipeLema/cmp-async-path",
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      local compare = require("cmp.config.compare")
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
        },
      })
      opts.experimental = {
        ghost_text = false,
      }
      -- opts.sorting = {
      --   priority_weight = 100,
      --   comparators = {
      --     deprio(require("cmp.types").lsp.CompletionItemKind.Text),
      --     compare.exact,
      --     compare.offset,
      --     compare.score,
      --     -- require('cmp-under-comparator').under,
      --     compare.recently_used,
      --     compare.kind,
      --     compare.locality,
      --     -- compare.sort_text,
      --     compare.length,
      --     compare.order,
      --   },
      -- }
    end,
  },
}
