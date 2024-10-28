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
    'AlexvZyl/nordic.nvim',
    enabled = true,
    event = 'VimEnter',
    config = function()
      require 'plugins.config.nordic'
    end,
  },
}
