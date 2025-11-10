---@type LazySpec
return {
  {
    "catppuccin",
    enabled = false,
    optional = true,
    opts = {
      ---@type "latte"| "frappe"| "macchiato"| "mocha" |"auto"
      flavour = "mocha",
      transparent_background = true,
      custom_highlights = function(colors)
        return {
          QuickScopePrimary = { fg = colors.yellow, bg = colors.surface1, style = { "underline", "bold" } },
          QuickScopeSecondary = { fg = colors.sapphire, bg = colors.surface1, style = { "underline", "bold" } },
        }
      end,
    },
  },
  {
    "vague2k/vague.nvim",
    enabled = true,
    lazy = true,
    opts = { transparent = false },
    specs = { { "LazyVim", opts = { colorscheme = "vague" } } },
    config = function(_, opts)
      require("vague").setup(opts)

      vim.api.nvim_create_autocmd("User", {
        group = vim.api.nvim_create_augroup("sp_load_late", { clear = true }),
        desc = 'Since we\'re loading colorscheme via "Lazyvim", need to apply overrides after everything is loaded i.e VeryLazy',
        pattern = "VeryLazy",
        once = true,
        callback = function()
          local c = require("vague").get_palette()
          vim.api.nvim_set_hl(0, "@tag.attribute", { fg = c.property })
          vim.api.nvim_set_hl(0, "WinBar", { bg = c.bg })
          vim.api.nvim_set_hl(0, "NavicSeparator", { fg = c.delta, bg = "NONE", bold = true })
          vim.api.nvim_set_hl(0, "SnacksPickerMatch", { fg = c.delta, bold = true })
          vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = c.delta, bg = c.visual, bold = true, undercurl = true })
          vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = c.hint, bg = c.visual, bold = true, undercurl = true })

          vim.api.nvim_set_hl(0, "CmpItemKindSnippet", { link = "@lsp.type.macro" })
          vim.api.nvim_set_hl(0, "CmpItemKindKeyword", { link = "@lsp.type.class" })
          vim.api.nvim_set_hl(0, "CmpItemKindText", { link = "@lsp.type.comment" })
          vim.api.nvim_set_hl(0, "CmpItemKindMethod", { link = "@lsp.type.method" })
          vim.api.nvim_set_hl(0, "CmpItemKindConstructor", { link = "@lsp.typemod.function" })
          vim.api.nvim_set_hl(0, "CmpItemKindFunction", { link = "@lsp.type.function" })
          vim.api.nvim_set_hl(0, "CmpItemKindFolder", { link = "@lsp.type.macro" })
          vim.api.nvim_set_hl(0, "CmpItemKindModule", { link = "@lsp.type.macro" })
          vim.api.nvim_set_hl(0, "CmpItemKindConstant", { link = "@lsp.type.builtinConstant" })
          vim.api.nvim_set_hl(0, "CmpItemKindField", { link = "@lsp.type.property" })
          vim.api.nvim_set_hl(0, "CmpItemKindProperty", { link = "@lsp.type.property" })
          vim.api.nvim_set_hl(0, "CmpItemKindEnum", { link = "@lsp.type.enum" })
          vim.api.nvim_set_hl(0, "CmpItemKindUnit", { link = "@lsp.type.typeParameter" })
          vim.api.nvim_set_hl(0, "CmpItemKindClass", { link = "@lsp.type.class" })
          vim.api.nvim_set_hl(0, "CmpItemKindVariable", { link = "@lsp.type.variable" })
          vim.api.nvim_set_hl(0, "CmpItemKindFile", { link = "@lsp.type.generic" })
          vim.api.nvim_set_hl(0, "CmpItemKindInterface", { link = "@lsp.type.interface" })
          vim.api.nvim_set_hl(0, "CmpItemKindColor", { link = "@lsp.type.generic" })
          vim.api.nvim_set_hl(0, "CmpItemKindReference", { link = "@lsp.typemod.variable.defaultLibrary" })
          vim.api.nvim_set_hl(0, "CmpItemKindEnumMember", { link = "@lsp.type.enumMember" })
          vim.api.nvim_set_hl(0, "CmpItemKindStruct", { link = "@lsp.type.class" })
          vim.api.nvim_set_hl(0, "CmpItemKindValue", { link = "@lsp.typemod.variable.definition" })
          vim.api.nvim_set_hl(0, "CmpItemKindEvent", { link = "@lsp.typemod.function.defaultLibrary" })
          vim.api.nvim_set_hl(0, "CmpItemKindOperator", { link = "@lsp.typemod.function.defaultLibrary" })
          vim.api.nvim_set_hl(0, "CmpItemKindTypeParameter", { link = "@lsp.type.builtinType" })
          vim.api.nvim_set_hl(0, "CmpItemKindCopilot", { link = "@lsp.type.comment" })
        end,
      })
    end,
  },
}
