---@type LazySpec
return {
  "NeogitOrg/neogit",
  cmd = "Neogit",
  keys = {
    { "<leader>gn", "<cmd>Neogit kind=floating<cr>", desc = "Neogit" },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "sindrets/diffview.nvim", cmd = "DiffviewOpen", opts = {} },
  },
  opts = {
    graph_style = "unicode",
    integrations = { snacks = true, diffview = true },
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
