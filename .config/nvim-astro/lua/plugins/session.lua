---@type LazySpec
return {
  "folke/persistence.nvim",
  event = "BufReadPre",
  opts = {},
  keys = {
    { "<leader>qs", function() require("persistence").select() end, desc = "Pick session" },
    { "<leader>ql", function() require("persistence").load() end, desc = "Load current directory" },
    { "<leader>qL", function() require("persistence").load { last = true } end, desc = "Load the last" },
    { "<leader>qd", function() require("persistence").stop() end, desc = "stop Persistence" },
  },
  specs = {
    { "stevearc/resession.nvim", enabled = false },
  },
}
