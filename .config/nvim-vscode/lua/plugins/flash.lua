---@type LazySpec
return {
  "folke/flash.nvim",
  opts = {
    modes = { char = { enabled = false, autohide = false } },
  },
  keys = {
    {
      "s",
      function()
        require("flash").jump()
      end,
      desc = "Flash",
      mode = { "n", "x" },
    },
    {
      "S",
      function()
        require("flash").treesitter()
      end,
      desc = "Treesitter",
      mode = { "n", "o" },
    },
    {
      "r",
      function()
        require("flash").remote()
      end,
      desc = "Remote",
      mode = "o",
    },
    {
      "<c-s>",
      function()
        require("flash").toggle()
      end,
      desc = "Toggle flash search",
      mode = "c",
    },
  },
}
