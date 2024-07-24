---@type LazySpec
return {
  {
    "LazyVim",
    opts = {
      colorscheme = "gruber-darker",
    },
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
    enabled = true,
    event = "VimEnter",
    config = function()
      require("config.gruber-darker")
    end,
  },
}
