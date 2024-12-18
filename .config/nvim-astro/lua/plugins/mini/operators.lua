---@type LazySpec
return {
  "echasnovski/mini.operators",
  version = "*",
  enabled = true,
  keys = {
    { "g=", mode = { "n", "x" }, desc = "Evalute" },
    { "ge", mode = { "n", "x" }, desc = "Exchange" },
    { "gm", mode = { "n", "x" }, desc = "Duplicate" },
    { "gR", mode = { "n", "x" }, desc = "Replace with register" },
    { "gs", mode = { "n", "x" }, desc = "Sort" },
  },
  opts = {
    -- Exchange text regions
    exchange = { prefix = "ge" },
    replace = { prefix = "gR" },
  },
  config = function(_, opts) require("mini.operators").setup(opts) end,
}
