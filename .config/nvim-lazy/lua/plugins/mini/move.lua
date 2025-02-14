---@type LazySpec
return {
  "echasnovski/mini.move",
  keys = {
    { "H", mode = "v" },
    { "J", mode = "v" },
    { "K", mode = "v" },
    { "L", mode = "v" },
    "<M-h>",
    "<M-j>",
    "<M-k>",
    "<M-l>",
  },
  opts = {
    -- Move current line in Visual mode
    mappings = {
      left = "H",
      right = "L",
      down = "J",
      up = "K",

      -- Move current line in Normal mode
      line_left = "<M-h>",
      line_right = "<M-l>",
      line_down = "<M-j>",
      line_up = "<M-k>",
    },
  },
}
