---@type LazySpec
return {
  "NeogitOrg/neogit",
  cmd = "Neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "sindrets/diffview.nvim", cmd = "DiffviewOpen", opts = {} },
  },
  keys = { { "<leader>gn", "<cmd>Neogit kind=floating<cr>", desc = "Neogit" } },
  opts = {
    integrations = { snacks = true, diffview = true },
    graph_style = "unicode",
    mappings = {
      commit_editor = {
        ["q"] = "Close",
        ["<c-c><c-c>"] = "Submit",
        ["<c-c><c-k>"] = "Abort",
        ["<c-c><c-p>"] = "PrevMessage",
        ["<c-c><c-n>"] = "NextMessage",
        ["<c-c><c-r>"] = "ResetMessage",
      },
    },
  },
}
