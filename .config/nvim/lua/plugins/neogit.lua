local gh = require("config.utils").gh

vim.pack.add({
  gh "NeogitOrg/neogit",
  gh "sindrets/diffview.nvim",
  gh "b0o/SchemaStore.nvim",
})

vim.keymap.set("n", "<leader>gn", "<cmd>Neogit kind=tab<cr>", { desc = "Neogit" })
vim.keymap.set("n", "<leader>og", "<cmd>Neogit kind=auto<cr>", { desc = "Neogit" })

vim.keymap.set("n", "<leader>gd", "<cmd>DiffviewOpen<cr>")
vim.keymap.set("n", "<leader>od", "<cmd>DiffviewOpen<cr>")
vim.keymap.set("n", "<leader>gD", "<cmd>DiffviewClose<cr>")
vim.keymap.set("n", "<leader>oD", "<cmd>DiffviewClose<cr>")

require("neogit").setup({
  graph_style = "unicode",
  integrations = {
    diffview = true,
    mini_pick = true,
  },
})
