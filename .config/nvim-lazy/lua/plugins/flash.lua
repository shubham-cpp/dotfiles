---@type LazySpec
return {
  "folke/flash.nvim",
  -- event = "VeryLazy",
  keys = {
    {
      "S",
      mode = { "n", "o" },
      function()
        require("flash").treesitter()
      end,
      desc = "Flash Treesitter",
    },
  },
  ---@type Flash.Config
  opts = {
    modes = { char = { enabled = false, autohide = false } },
  },
}
