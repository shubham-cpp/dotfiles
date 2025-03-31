---@type LazySpec
return {
  {
    "LazyVim",
    opts = { colorscheme = "catppuccin" },
  },
  {
    "catppuccin",
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
    lazy = true,
    opts = { transparent = false },
    config = function(_, opts)
      require("vague").setup(opts)
      local c = require("vague").get_palette()

      vim.api.nvim_set_hl(0, "@tag.attribute", { fg = c.property })
      vim.api.nvim_set_hl(0, "WinBar", { bg = c.bg })
      vim.api.nvim_set_hl(0, "SnacksPickerMatch", { fg = c.delta, bold = true })
      vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = c.delta, bg = c.visual, bold = true, undercurl = true })
      vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = c.hint, bg = c.visual, bold = true, undercurl = true })
    end,
  },
}
