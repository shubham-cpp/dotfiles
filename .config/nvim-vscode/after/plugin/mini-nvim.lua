local ok, operators = pcall(require, "mini.operators")
if not ok then
  return
end

require("mini.align").setup({})
require("mini.move").setup({
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
})

operators.setup({
  exchange = { prefix = "ge" },
  replace  = { prefix = "x" },
})
vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
