---@type LazySpec
return {
  "danymat/neogen",
  cmd = "Neogen",
  keys = {
    { "<leader>n", "", desc = "+neogen" },
    { "<leader>nn", "<cmd>Neogen<cr>", desc = "Default" },
    { "<leader>nt", "<cmd>Neogen type<cr>", desc = "Type" },
    { "<leader>nf", "<cmd>Neogen file<cr>", desc = "File" },
    { "<leader>nc", "<cmd>Neogen class<cr>", desc = "Class" },
  },
  opts = {
    snippet_engine = "luasnip",
  },
}
