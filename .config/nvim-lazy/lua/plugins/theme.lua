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
        end,
      })
    end,
  },
}
