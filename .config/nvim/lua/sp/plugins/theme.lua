local config = {
  {
    'rmehri01/onenord.nvim',
    enabled = true,
    opts = {
      fade_nc = true,
      disable = {
        background = true,
      },
      styles = {
        comments = 'italic',
        strings = 'NONE',
        keywords = 'NONE',
        functions = 'bold',
        variables = 'NONE',
        diagnostics = 'underline',
      },
      custom_highlights = {
        -- Normal = { bg = '#1e222a' },
        -- NormalNC = { bg = '#1d1915' },
        QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = 'underline,bold' },
        QuickScopeSecondary = {
          fg = '#61afef',
          bg = '#505050',
          style = 'underline,bold',
        },
        PmenuSel = { bg = '#61afef', fg = '#24253b' },
      },
    },
  },
  {
    'navarasu/onedark.nvim',
    enabled = false,
    config = function()
      require('onedark').setup({
        style = 'cool', -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
        transparent = true,
        -- cmp_itemkind_reverse = false
      })
      require('onedark').load()
    end,
  },
  {
    'ellisonleao/gruvbox.nvim',
    enabled = false,
    config = function()
      require('gruvbox').setup({
        contrast = 'hard', -- can be "hard", "soft" or empty string
        dim_inactive = false,
        transparent_mode = true,
        overrides = {
          QuickScopePrimary = {
            fg = '#d3869b',
            bg = '#505050',
            bold = true,
            italic = true,
          },
          QuickScopeSecondary = {
            fg = '#61afef',
            bg = '#505050',
            bold = true,
            italic = true,
          },
        },
      })
      vim.cmd 'colorscheme gruvbox'
    end,
  },
  {
    'sainnhe/gruvbox-material',
    enabled = false,
    init = function()
      vim.g.gruvbox_material_better_performance = 1
      vim.g.gruvbox_material_background = 'hard' -- 'hard', 'medium'(default), 'soft'
      vim.g.gruvbox_material_foreground = 'material' -- 'material'(default), 'mix', 'original'
      vim.g.gruvbox_material_enable_bold = 1
      vim.g.gruvbox_material_enable_italic = 1
      vim.g.gruvbox_material_transparent_background = 1 -- 0, 1, 2
      vim.g.gruvbox_material_dim_inactive_windows = 1
      vim.g.gruvbox_material_diagnostic_text_highlight = 1
      vim.g.gruvbox_material_diagnostic_line_highlight = 1
    end,
    config = function()
      vim.cmd 'colorscheme gruvbox-material'
    end,
  },
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    enabled = false,
    opts = {
      flavour = 'macchiato', -- latte, frappe, macchiato, mocha
      transparent_background = true,
      custom_highlights = function(colors)
        return {
          QuickScopePrimary = {
            fg = '#d3869b',
            bg = '#505050',
            bold = true,
            italic = true,
          },
          QuickScopeSecondary = {
            fg = '#61afef',
            bg = '#505050',
            bold = true,
            italic = true,
          },
        }
      end,
      integrations = {
        flash = false,
        leap = true,
        neotree = true,
        mason = true,
        navic = { enabled = true },
        window_picker = true,
        which_key = true,
      },
    },
    config = function(_, opts)
      require('catppuccin').setup(opts)
      vim.cmd.colorscheme 'catppuccin'
    end,
  },
}

return config
