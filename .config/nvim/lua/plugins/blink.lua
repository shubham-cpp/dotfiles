return {
  {
    "saghen/blink.cmp",
    event = "InsertEnter",
    dependencies = { "L3MON4D3/LuaSnip", "mikavilpas/blink-ripgrep.nvim" },
    sem_version = "^1",
    config = function()
      require("blink.cmp").setup({
        appearance = {
          nerd_font_variant = "mono",
        },
        keymap = {
          preset = "enter",
          ["<CR>"] = { "select_and_accept", "fallback" },
          ["<C-j>"] = { "select_next", "fallback" },
          ["<C-k>"] = { "select_prev", "fallback" },
          ["<C-n>"] = { "select_next", "snippet_forward", "fallback" },
          ["<C-p>"] = { "select_prev", "snippet_backward", "fallback" },
          ["<C-h>"] = { "show_signature", "hide_signature", "fallback" },
          ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
          ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
        },
        completion = {
          list = { selection = { preselect = false, auto_insert = true } },
          ghost_text = { enabled = false },
          documentation = {
            auto_show = true,
            auto_show_delay_ms = 400,
            window = { border = "none" },
          },
          menu = {
            border = "none",
            scrollbar = false,
            draw = {
              padding = { 0, 1 },
              treesitter = { "lsp" },
              columns = {
                { "kind_icon" },
                { "label", "label_description", gap = 1 },
              },
              components = {
                kind_icon = {
                  text = function(ctx)
                    local ok, mini_icons = pcall(require, "mini.icons")
                    if ok then
                      if ctx.item.source_name == "LSP" then
                        local icon, _ = mini_icons.get("lsp", ctx.kind or "")
                        if icon then
                          ctx.kind_icon = icon
                        end
                      elseif ctx.item.source_name == "Path" then
                        local icon, _ = mini_icons.get(ctx.kind == "Folder" and "directory" or "file", ctx.label)
                        if icon then
                          ctx.kind_icon = icon
                        end
                      elseif ctx.item.source_name == "Snippets" then
                        local icon, _ = mini_icons.get("lsp", "snippet")
                        if icon then
                          ctx.kind_icon = icon
                        end
                      end
                    end
                    return " " .. ctx.kind_icon .. " "
                  end,
                  highlight = function(ctx)
                    local ok, mini_icons = pcall(require, "mini.icons")
                    if ok then
                      local _, hl
                      if ctx.item.source_name == "LSP" then
                        _, hl = mini_icons.get("lsp", ctx.kind or "")
                      elseif ctx.item.source_name == "Path" then
                        _, hl = mini_icons.get(ctx.kind == "Folder" and "directory" or "file", ctx.label)
                      end
                      if hl then
                        return hl
                      end
                    end
                    return ctx.kind_hl
                  end,
                },
              },
            },
          },
          accept = { auto_brackets = { enabled = true } },
        },
        sources = {
          default = { "lsp", "path", "snippets", "buffer", "ripgrep" },
          providers = {
            lsp = { fallbacks = {} },
            buffer = { score_offset = -5, min_keyword_length = 3 },
            ripgrep = {
              module = "blink-ripgrep",
              name = "Ripgrep",
              score_offset = -8,
              max_items = 20,
              opts = {
                prefix_min_len = 4,
                project_root_marker = { ".git", "package.json", "pyproject.toml", "Cargo.toml", ".root" },
                backend = {
                  use = "gitgrep-or-ripgrep",
                  ripgrep = {
                    search_casing = "--smart-case",
                    max_filesize = "1M",
                  },
                },
              },
            },
            cmdline = {
              enabled = function()
                return vim.fn.getcmdtype() ~= ":" or not vim.fn.getcmdline():match("^[%%0-9,'<>%-]*!")
              end,
            },
          },
        },
        snippets = { preset = "luasnip" },
        fuzzy = {
          implementation = "prefer_rust",
          sorts = {
            -- function(a, b)
            --   local function is_emmet(source)
            --     return source.client_name == "emmet_ls" or source.client_name == "emmet_language_server"
            --   end
            --   local a_emmet = is_emmet(a)
            --   local b_emmet = is_emmet(b)
            --
            --   if a_emmet ~= b_emmet then
            --     return not a_emmet
            --   end
            -- end,
            "score",
            "sort_text",
            "exact",
          },
        },
        signature = { enabled = true, window = { border = "rounded" } },
        cmdline = {
          keymap = {
            preset = "cmdline",
            ["<Left>"] = { "fallback" },
            ["<Right>"] = { "fallback" },
            ["<C-j>"] = { "select_next", "fallback" },
            ["<C-k>"] = { "select_prev", "fallback" },
          },
          completion = {
            list = { selection = { preselect = false } },
            menu = { auto_show = true },
          },
          sources = function()
            local type = vim.fn.getcmdtype()
            if type == "/" or type == "?" then
              return { "buffer" }
            end
            return { "buffer", "cmdline" }
          end,
        },
      })
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    dependencies = { "rafamadriz/friendly-snippets" },
    sem_version = "^2",
    config = function()
      require("luasnip").setup({
        delete_check_events = "TextChanged",
        region_check_events = "CursorMoved",
      })
      require("luasnip.loaders.from_vscode").lazy_load({
        paths = { vim.fn.stdpath("config") .. "/snippets" },
        override_priority = 2000,
      })
      require("luasnip.loaders.from_vscode").lazy_load({
        default_priority = 1000,
      })
    end,
  },
}
