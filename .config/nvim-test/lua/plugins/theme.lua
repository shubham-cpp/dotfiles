---@type LazySpec
return {
  {
    'sainnhe/gruvbox-material',
    event = 'VimEnter',
    enabled = false,
    init = function()
      vim.g.gruvbox_material_background = 'hard' -- 'hard', 'medium'(default), 'soft'
      vim.g.gruvbox_material_enable_bold = 1
      vim.g.gruvbox_material_dim_inactive_windows = 1
      vim.g.gruvbox_material_transparent_background = 2 -- 0, 1, 2
      vim.g.gruvbox_material_better_performance = 1
    end,
    config = function()
      local configuration = vim.fn['gruvbox_material#get_configuration']()
      local palette = vim.fn['gruvbox_material#get_palette'](
        configuration.background,
        configuration.foreground,
        configuration.colors_override
      )
      vim.cmd.colorscheme 'gruvbox-material'
      vim.cmd(string.format('hi! MiniTablineCurrent guifg=%s guibg=%s gui=bold', palette.bg0[1], palette.grey2[1]))
    end,
  },
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    event = 'VimEnter',
    enabled = true,
    config = function()
      require 'plugins.config.catppuccin'
    end,
  },
  {
    'rebelot/kanagawa.nvim',
    event = 'VimEnter',
    enabled = false,
    config = function()
      require('kanagawa').setup({
        compile = true,
        -- transparent = true, -- do not set background color
        dimInactive = true,
        theme = 'wave', -- Load "wave" | "dragon" | "lotus"
      })
      vim.cmd 'colorscheme kanagawa-wave'
    end,
  },
  {
    'rmehri01/onenord.nvim',
    enabled = false,
    event = 'VimEnter',
    config = function()
      require 'plugins.config.onenord'
    end,
  },
}