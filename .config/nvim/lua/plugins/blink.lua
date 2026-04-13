return {
  {
    url = "saghen/blink.cmp",
    version = vim.version.range("^1"),
    config = function()
      require("blink.cmp").setup({
        keymap = {
          preset = "enter",
          ["<CR>"] = { "select_and_accept", "fallback" },
          ["<C-j>"] = { "select_next", "fallback" },
          ["<C-k>"] = { "select_prev", "fallback" },
          ["<C-s>"] = { "snippet_forward", "fallback" },
          ["<C-h>"] = { "show_signature", "hide_signature", "fallback" },
          ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
          ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
        },

        completion = {
          list = { selection = { preselect = false, auto_insert = false } },
          ghost_text = { enabled = false },
          documentation = {
            auto_show = true,
            auto_show_delay_ms = 200,
            window = {
              border = "none",
            },
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
                -- { "kind" },
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
                -- kind = {
                --   text = function(ctx)
                --     return " " .. ctx.kind .. " "
                --   end,
                --   highlight = function(ctx)
                --     return "BlinkCmpKind" .. (ctx.kind or "")
                --   end,
                -- },
              },
            },
          },
          accept = {
            auto_brackets = {
              enabled = true,
            },
          },
        },

        sources = {
          default = { "lsp", "path", "snippets", "buffer", "ripgrep" },
          providers = {
            buffer = {
              score_offset = -3,
            },
            ripgrep = {
              module = "blink-ripgrep",
              name = "Ripgrep",
              opts = {
                prefix_min_len = 4,
                score_offset = -5,
                search_casing = "--smart-case",
              },
            },
            cmdline = {
              enabled = function()
                return vim.fn.getcmdtype() ~= ":" or not vim.fn.getcmdline():match("^[%%0-9,'<>%-]*!")
              end,
            },
          },
        },

        snippets = {
          preset = "luasnip",
        },

        fuzzy = {
          implementation = "prefer_rust",
          sorts = { "exact", "score", "sort_text" },
        },

        signature = {
          enabled = true,
          window = {
            border = "rounded",
          },
        },

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

  -- Snippet engine
  {
    url = "L3MON4D3/LuaSnip",
    version = vim.version.range("^2"),
    config = function()
      require("luasnip").setup({
        history = true,
        delete_check_events = "TextChanged",
      })
      -- Load VSCode-style snippets from local snippets/ directory
      require("luasnip.loaders.from_vscode").lazy_load({
        paths = { vim.fn.stdpath("config") .. "/snippets" },
      })
      -- Load VSCode-style snippets from friendly-snippets
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },

  -- Community snippet collection
  {
    url = "rafamadriz/friendly-snippets",
  },

  -- Ripgrep source for blink.cmp (configured as source inside blink setup)
  {
    url = "mikavilpas/blink-ripgrep.nvim",
  },
}
