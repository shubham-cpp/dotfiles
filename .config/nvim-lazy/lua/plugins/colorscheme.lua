---@type LazySpec
return {
  {
    "LazyVim",
    opts = {
      colorscheme = "cyberdream",
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
    "scottmckendry/cyberdream.nvim",
    enabled = true,
    event = "VimEnter",
    config = function()
      require("cyberdream").setup({
        transparent = true,
        cache = true,
        theme = {
          highlights = {
            QuickScopePrimary = { fg = "#5eff6c", bg = "#181818", bold = true, underline = true },
            QuickScopeSecondary = { fg = "#ffbd5e", bg = "#181818", bold = true, underline = true },
          },
        },
      })
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
