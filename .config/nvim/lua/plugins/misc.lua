local gh = require("config.utils").gh

vim.g.qs_highlight_on_keys = { "f", "F", "t", "T" }
vim.g.qs_lazy_highlight = 1
vim.g.qs_buftype_blacklist = { "terminal", "nofile", "dashboard", "startify" }

vim.pack.add({
  gh "unblevable/quick-scope",
  gh "pteroctopus/faster.nvim",
})

vim.api.nvim_set_hl(0, "EyelinerPrimary", { fg = "#f3be7c", bold = true, underline = true })
vim.api.nvim_set_hl(0, "EyelinerSecondary", { fg = "#7e98e8", underline = true })
vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#f3be7c", bold = true, underline = true })
vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "#7e98e8", underline = true })

require("faster").setup()
