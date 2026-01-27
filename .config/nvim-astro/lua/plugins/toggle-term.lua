---@type LazySpec
return {
  "akinsho/toggleterm.nvim",
  optional = true,
  keys = { "<C-\\>" },
  opts = {
    open_mapping = [[<c-\>]],
    direction = "float",
    shell = "fish",
    on_open = function(t)
      local bufnr = t.bufnr
      -- vim.opt_local.foldexpr = ""
      -- vim.opt_local.foldmethod = "manual"
      vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr, desc = "Goto normal mode" })
      vim.keymap.set("n", "A", "A<C-k>", { buffer = bufnr })
      vim.keymap.set("n", "D", "A<C-k><C-\\><C-n>", { buffer = bufnr })
      vim.keymap.set("n", "cc", "A<C-e><C-u>", { buffer = bufnr })
      vim.keymap.set("n", "dd", "A<C-e><C-u><C-\\><C-n>", { buffer = bufnr })
    end,
  },
}
