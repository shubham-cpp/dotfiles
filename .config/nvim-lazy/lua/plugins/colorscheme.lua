---@type LazySpec
return {
  {
    "LazyVim",
    opts = { colorscheme = "nordic" },
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
  {
    "AlexvZyl/nordic.nvim",
    enabled = true,
    event = "VimEnter",
    config = function()
      require("config.nordic")
    end,
  },
}
