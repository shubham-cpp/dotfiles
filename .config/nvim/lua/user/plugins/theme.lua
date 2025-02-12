---@type LazySpec
return {
  {
    'catppuccin/nvim',
    enabled = false,
    name = 'catppuccin',
    priority = 1000,
    opts = {
      ---@type 'latte'|'frappe'| 'macchiato'| 'mocha'
      flavour = 'mocha',
      transparent_background = true, -- disables setting the background color.
      -- dim_inactive = { enabled = true },
      integrations = {
        blink_cmp = true,
        dadbod_ui = true,
        diffview = true,
        navic = { enabled = true },
        snacks = true,
        which_key = true,
        window_picker = true,
      },
      custom_highlights = function()
        return {
          QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = { 'underline', 'bold' } },
          QuickScopeSecondary = { fg = '#61afef', bg = '#505050', style = { 'underline', 'bold' } },
        }
      end,
    },
    config = function(_, opts)
      require('catppuccin').setup(opts)
      vim.cmd.colorscheme 'catppuccin'
    end,
  },
  {
    'olivercederborg/poimandres.nvim',
    enabled = false,
    priority = 1000,
    opts = {
      bold_vert_split = false, -- use bold vertical separators
      dim_nc_background = false, -- dim 'non-current' window backgrounds
      disable_background = true, -- disable background
      disable_float_background = false, -- disable background for floats
      disable_italics = false, -- disable italics
    },
    config = function(_, opts)
      require('poimandres').setup(opts)
      vim.cmd.colorscheme 'poimandres'
      vim.api.nvim_set_hl(0, 'LspReferenceRead', { bg = 'NvimDarkGray4', bold = true })
      vim.api.nvim_set_hl(0, 'LspReferenceText', { bg = 'NvimDarkGray4', bold = true })
      vim.api.nvim_set_hl(0, 'LspReferenceWrite', { bg = 'NvimDarkGray4', bold = true })
    end,
  },
  {
    'vague2k/vague.nvim',
    branch = '1.3',
    enabled = false,
    priority = 1000,
    opts = { transparent = false },
    config = function(_, opts)
      require('vague').setup(opts)
      vim.cmd.colorscheme 'vague'
      vim.api.nvim_set_hl(0, 'StatusLine', { bg = '#181818' })
    end,
  },
  {
    'AlexvZyl/nordic.nvim',
    enabled = false,
    priority = 1000,
    opts = {
      transparent = { bg = true },
      bright_border = true,
      on_highlight = function(highlights, palette)
        highlights.Visual = {
          bg = palette.gray1,
          bold = true,
        }
        highlights['QuickScopePrimary'] = {
          fg = palette.yellow.bright,
          bg = palette.gray0,
          bold = true,
          underline = true,
        }
        highlights['QuickScopeSecondary'] = {
          fg = palette.orange.dim,
          bg = palette.gray0,
          bold = true,
          underline = true,
        }
        highlights['@tag.attribute'] = {
          fg = '#6cc3b7', -- #6dada4 |
        }
      end,
    },
    config = function(_, opts)
      require('nordic').setup(opts)
      vim.cmd.colorscheme 'nordic'
    end,
  },
  {
    'EdenEast/nightfox.nvim',
    enabled = true,
    priority = 1000,
    opts = {
      options = {
        transparent = true,
        styles = {
          comments = 'italic',
        },
      },
    },
    config = function(_, opts)
      require('nightfox').setup(opts)
      vim.cmd.colorscheme 'duskfox'
    end,
  },
  {
    'wtfox/jellybeans.nvim',
    enabled = false,
    priority = 1000,
    opts = {
      style = 'dark', -- "dark" or "light"
      transparent = true,
    },
    config = function(_, opts)
      require('jellybeans').setup(opts)
      vim.cmd.colorscheme 'jellybeans'
    end,
  },
}
