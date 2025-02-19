---@type LazySpec
return {
  {
    "echasnovski/mini.operators",
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gS", mode = { "n", "x" }, desc = "Sort" },
      "X",
    },
    opts = {
      -- Exchange text regions
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
      sort = { prefix = "gS" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
  {
    "echasnovski/mini.move",
    keys = {
      { "<", mode = { "n", "v" } },
      { ">", mode = { "n", "v" } },
      { "J", mode = "v" },
      { "K", mode = "v" },
    },
    opts = {
      mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = "<",
        right = ">",
        down = "J",
        up = "K",

        -- Move current line in Normal mode
        line_left = "<",
        line_right = ">",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
    -- config = function(_, opts) require("mini.move").setup(opts) end,
  },
  {
    "echasnovski/mini.align",
    keys = { "ga", "gA" },
    opts = {},
  },
}
