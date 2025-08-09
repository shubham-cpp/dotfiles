---@type LazySpec
return {
  "folke/trouble.nvim",
  optional = true,
  keys = {
    { "<leader>cs", false },
    { "<leader>cS", false },
    { "<leader>ls", "<cmd>Trouble symbols toggle<cr>", desc = "Symbols (Trouble)" },
    { "<leader>lS", "<cmd>Trouble lsp toggle<cr>", desc = "LSP references/definitions/... (Trouble)" },
  },
}
