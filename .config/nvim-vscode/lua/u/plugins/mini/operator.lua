local later = MiniDeps.later
later(function()
  require("mini.operators").setup({
    -- Exchange text regions
    exchange = { prefix = "ge" },
    replace = { prefix = "x" },
  })
  vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
end)
