---@type LazySpec
return {
  "folke/which-key.nvim",
  opts = {
    spec = {
      {
        mode = { "n", "v" },
        { "<leader>l", group = "lsp" },
      },
    },
  },
}
