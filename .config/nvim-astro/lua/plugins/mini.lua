---@type LazySpec
return {
  {
    "nvim-mini/mini.move",
    keys = {
      { "<", mode = "x", desc = "Move selection left" },
      { ">", mode = "x", desc = "Move selection right" },
      { "J", mode = "x", desc = "Move selection down" },
      { "K", mode = "x", desc = "Move selection up" },
      { "<M-h>", mode = { "n", "x" }, desc = "Move line left" },
      { "<M-l>", mode = { "n", "x" }, desc = "Move line right" },
      { "<M-j>", mode = { "n", "x" }, desc = "Move line down" },
      { "<M-k>", mode = { "n", "x" }, desc = "Move line up" },
    },
    opts = {
      mappings = {
        left = "<",
        right = ">",
        down = "J",
        up = "K",
        line_left = "<M-h>",
        line_right = "<M-l>",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
  },
  {
    "nvim-mini/mini.operators",
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evaluate operator" },
      { "ge", mode = { "n", "x" }, desc = "Exchange operator" },
      { "gm", mode = { "n", "x" }, desc = "Multiply operator" },
      { "gs", mode = { "n", "x" }, desc = "Sort operator" },
      { "x", mode = { "n", "x" }, desc = "Replace operator" },
      { "X", "x$", desc = "Replace to end of line", remap = true },
    },
    opts = {
      evaluate = { prefix = "g=" },
      exchange = { prefix = "ge" },
      multiply = { prefix = "gm" },
      replace = { prefix = "x" },
      sort = { prefix = "gs" },
    },
  },
}
