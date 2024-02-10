return {
  "kylechui/nvim-surround",
  config = true,
  -- config = function()
  --   require("nvim-surround").setup()
  -- end,
  keys = { "ys", "ds", "cs", { "S", mode = "x", silent = false } },
}
