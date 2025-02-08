---@type LazySpec
return {
  {
    "LazyVim",
    opts = { colorscheme = "nordic" },
  },
  -- {
  --   "lualine.nvim",
  --   opts = {
  --     options = {
  --       theme = "iceberg_dark",
  --     },
  --   },
  -- },
  {
    "vague2k/vague.nvim",
    enabled = false,
    event = "VimEnter",
    opts = { transparent = false },
    config = function(_, opts)
      require("vague").setup(opts)
    end,
  },
  {
    "AlexvZyl/nordic.nvim",
    enabled = true,
    event = "VimEnter",
    config = function()
      require("config.nordic")
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    enabled = false,
    event = "VimEnter",
    config = function()
      require("config.kangawa")
    end,
  },
  {
    "blazkowolf/gruber-darker.nvim",
    enabled = false,
    event = "VimEnter",
    config = function()
      require("config.gruber-darker")
    end,
  },
}
