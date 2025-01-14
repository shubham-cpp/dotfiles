---@type LazySpec
return {
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    event = 'VimEnter',
    enabled = true,
    config = function()
      require 'my_config.catppuccin'
    end,
  },
  {
    'rebelot/kanagawa.nvim',
    event = 'VimEnter',
    enabled = false,
    config = function()
      require 'my_config.kangawa'
    end,
  },
  {
    'blazkowolf/gruber-darker.nvim',
    enabled = false,
    event = 'VimEnter',
    config = function()
      require 'my_config.gruber-darker'
    end,
  },
  {
    'folke/tokyonight.nvim',
    enabled = false,
    event = 'VimEnter',
    config = function()
      require 'my_config.tokyonight'
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
    enabled = false,
    event = 'VimEnter',
    config = function()
      require 'my_config.nordic'
    end,
  },
}
