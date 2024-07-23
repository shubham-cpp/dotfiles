---@type LazySpec
return {
  {
    "blazkowolf/gruber-darker.nvim",
    enabled = true,
    event = "VimEnter",
    config = function()
      require("config.gruber-darker")
    end,
  },
}
