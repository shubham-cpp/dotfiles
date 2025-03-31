local function has_words_before()
  local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match "%s" == nil
end

---@type LazySpec
return {
  "Saghen/blink.cmp",
  version = "*",
  event = { "InsertEnter", "CmdlineEnter" },
  dependencies = {
    "mikavilpas/blink-ripgrep.nvim",
    {
      "folke/lazydev.nvim",
      ft = "lua", -- only load on lua files
      dependencies = { "Bilal2453/luvit-meta" },
      opts = {
        library = {
          "lazy.nvim",
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        },
      },
    },
    "L3MON4D3/LuaSnip",
  },
  ---@module 'blink.cmp'
  ---@type blink.cmp.Config
  opts = {
    sources = {
      default = {
        "lazydev",
        "lsp",
        "snippets",
        "path",
        "buffer",
        "ripgrep",
        "markdown",
        -- 'dadbod',
      },
      providers = {
        lazydev = {
          name = "LazyDev",
          module = "lazydev.integrations.blink",
          -- make lazydev completions top priority (see `:h blink.cmp`)
          -- score_offset = 100,
        },
        lsp = {
          name = "LSP",
          module = "blink.cmp.sources.lsp",
          -- fallbacks = { 'lazydev' },
          -- score_offset = 150, -- the higher the number, the higher the priority
          -- Filter text items from the LSP provider, since we have the buffer provider for that
          transform_items = function(_, items)
            -- for _, item in ipairs(items) do
            --   if item.kind == require('blink.cmp.types').CompletionItemKind.Snippet then
            --     item.score_offset = item.score_offset - 50
            --   end
            -- end

            return vim.tbl_filter(
              function(item) return item.kind ~= require("blink.cmp.types").CompletionItemKind.Text end,
              items
            )
          end,
        },
        ripgrep = {
          module = "blink-ripgrep",
          name = "Ripgrep",
          ---@module "blink-ripgrep"
          ---@type blink-ripgrep.Options
          opts = {
            prefix_min_len = 4,
            -- score_offset = 10, -- should be lower priority
            max_filesize = "300K",
            search_casing = "--smart-case",
          },
        },
        markdown = {
          name = "RenderMarkdown",
          module = "render-markdown.integ.blink",
          fallbacks = { "lsp" },
        },
      },
    },
    snippets = { preset = "luasnip" },
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
    },
    keymap = {
      preset = "enter",
      ["<CR>"] = { "select_and_accept", "fallback" },
      ["<C-y>"] = { "select_and_accept", "fallback" },
      ["<C-J>"] = { "select_next", "fallback" },
      ["<C-K>"] = { "select_prev", "fallback" },
      ["<Tab>"] = {
        "snippet_forward",
        "select_next",
        function(cmp)
          if has_words_before() or vim.api.nvim_get_mode().mode == "c" then return cmp.show() end
        end,
        "fallback",
      },
      ["<S-Tab>"] = {
        "snippet_backward",
        "select_prev",
        function(cmp)
          if vim.api.nvim_get_mode().mode == "c" then return cmp.show() end
        end,
        "fallback",
      },
    },
    completion = {
      list = { selection = { preselect = false, auto_insert = true } },
      accept = { auto_brackets = { enabled = true } },
      documentation = {
        auto_show = true,
        auto_show_delay_ms = 150,
        window = {
          border = "rounded",
          winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",
        },
      },
      menu = {
        draw = { treesitter = { "lsp" } },
        border = "rounded",
        winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",
      },
    },
    signature = {
      window = {
        border = "rounded",
        winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder",
      },
    },
  },
  opts_extend = { "sources.default", "cmdline.sources", "term.sources" },
}
