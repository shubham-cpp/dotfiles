local u = require "config.utils"

vim.pack.add({ u.gh "danymat/neogen" })

require("neogen").setup({
  snippet_engine = "mini",
})

vim.keymap.set("n", "<leader>nf", "<cmd>Neogen func<cr>", { desc = "Function" })
vim.keymap.set("n", "<leader>nc", "<cmd>Neogen class<cr>", { desc = "Class" })
