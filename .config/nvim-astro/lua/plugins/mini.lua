---@type LazySpec
return {
  {
    "nvim-mini/mini.move",
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
    opts = {
      evaluate = { prefix = "g=" },
      exchange = { prefix = "ge" },
      multiply = { prefix = "gm" },
      replace = { prefix = "x" },
      sort = { prefix = "gs" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
}
