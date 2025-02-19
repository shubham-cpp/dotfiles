---@type LazySpec
return {
  "NeogitOrg/neogit",
  cmd = "Neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "sindrets/diffview.nvim", cmd = "DiffviewOpen", opts = {} }, -- optional - Diff integration
  },
  keys = {
    { "<leader>gn", "<cmd>Neogit kind=floating<cr>", desc = "Neogit" },
  },
  opts = {},
}
