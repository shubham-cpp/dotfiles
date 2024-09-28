---@type LazySpec
return {
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    event = 'VimEnter',
    enabled = false,
    config = function()
      require 'plugins.config.catppuccin'
    end,
  },
  {
    'rebelot/kanagawa.nvim',
    event = 'VimEnter',
    enabled = false,
    config = function()
      require 'plugins.config.kangawa'
    end,
  },
  {
    'blazkowolf/gruber-darker.nvim',
    enabled = false,
    event = 'VimEnter',
    config = function()
      require 'plugins.config.gruber-darker'
    end,
  },
  {
    'folke/tokyonight.nvim',
    enabled = false,
    event = 'VimEnter',
    config = function()
      require 'plugins.config.tokyonight'
    end,
  },
  {
    'rose-pine/neovim',
    enabled = false,
    event = 'VimEnter',
    name = 'rose-pine',
    config = function()
      require('rose-pine').setup({
        styles = {
          transparency = true,
        },
      })
      vim.cmd.colorscheme 'rose-pine'
    end,
  },
  {
    'gmr458/vscode_modern_theme.nvim',
    enabled = true,
    event = 'VimEnter',
    config = function()
      require('vscode_modern').setup({
        cursorline = true,
        transparent_background = false,
      })
      vim.cmd.colorscheme 'vscode_modern'

      vim.api.nvim_set_hl(0, 'QuickScopePrimary', { fg = '#E5C324', bg = '#121212', underline = true, bold = true })
      vim.api.nvim_set_hl(0, 'QuickScopeSecondary', { fg = '#53D14E', bg = '#121212', underline = true, bold = true })
    end,
  },
}
