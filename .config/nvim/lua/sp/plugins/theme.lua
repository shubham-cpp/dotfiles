local config = {
  {
    'rmehri01/onenord.nvim',
    enabled = false,
    opts = {
      fade_nc = true,
      styles = {
        comments = 'italic',
        strings = 'NONE',
        keywords = 'NONE',
        functions = 'bold',
        variables = 'NONE',
        diagnostics = 'underline',
      },
      custom_highlights = {
        Normal = { bg = '#1e222a' },
        NormalNC = { bg = '#1d1915' },
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
    'ellisonleao/gruvbox.nvim',
    enabled = false,
    config = function()
      require('gruvbox').setup({
        contrast = 'hard', -- can be "hard", "soft" or empty string
        dim_inactive = true,
        transparent_mode = false,
        overrides = {
          QuickScopePrimary = {
            fg = '#dfbb78',
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
    'navarasu/onedark.nvim',
    enabled = false,
    config = function()
      require('onedark').setup({
        style = 'darker', -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
        transparent = false,
        -- cmp_itemkind_reverse = false
      })
      require('onedark').load()
    end,
  },
  {
    'projekt0n/caret.nvim',
    enabled = false,
    config = function()
      require('caret').setup({
        options = {
          transparent = false, -- Set to true to disable background setting
          -- inverted_signs = false,    -- Controls inverted Signcolumn highlighting
          -- styles = {                 -- Define styles for various syntax groups
          --   bold = true,
          --   italic = true,
          --   strikethrough = true,
          --   undercurl = true,
          --   underline = true,
          -- },
          inverse = { -- Determines inverse highlights for different types
            match_paren = true,
            visual = true,
            search = true,
          },
        },
        groups = {}, -- Override default highlight groups here
      })
      vim.cmd 'colorscheme caret'
    end,
  },
  {
    'sainnhe/gruvbox-material',
    enabled = true,
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
}

return config
