---@type LazySpec
return {
  {
    "echasnovski/mini.align",
    version = "*",
    keys = { "ga", "gA" },
    opts = {},
  },
  {
    "echasnovski/mini.move",
    keys = {
      { "<", mode = "v" },
      { "J", mode = "v" },
      { "K", mode = "v" },
      { ">", mode = "v" },
      "<M-h>",
      "<M-j>",
      "<M-k>",
      "<M-l>",
    },
    opts = {
      -- Move current line in Visual mode
      mappings = {
        left = "<",
        right = ">",
        down = "J",
        up = "K",

        -- Move current line in Normal mode
        line_left = "<M-h>",
        line_right = "<M-l>",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
  },
  {
    "echasnovski/mini.operators",
    version = "*",
    enabled = true,
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gs", mode = { "n", "x" }, desc = "Sort" },
      "X",
    },
    opts = {
      -- Exchange text regions
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
}
