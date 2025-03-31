---@type LazySpec
return {
  "danymat/neogen",
  cmd = "Neogen",
  keys = {
    -- { '<leader>n', desc = 'Neogen', noremap = false },
    { "<leader>nf", "<cmd>Neogen func<cr>", desc = "Func" },
    { "<leader>nt", "<cmd>Neogen type<cr>", desc = "Type" },
    { "<leader>nm", "<cmd>Neogen file<cr>", desc = "Module" },
    { "<leader>nc", "<cmd>Neogen class<cr>", desc = "Class" },
  },
  opts = {
    ---@type 'luasnip'| 'snippy'| 'vsnip'| 'nvim'
    snippet_engine = "luasnip",
  },
}
