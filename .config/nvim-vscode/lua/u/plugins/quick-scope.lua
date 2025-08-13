local add, later = MiniDeps.add, MiniDeps.later
later(function()
  vim.g.qs_highlight_on_keys = { "f", "F", "t", "T" }
  vim.g.qs_buftype_blacklist = { "terminal", "nofile", "dashboard", "startify" }
  vim.g.qs_lazy_highlight = 1

  add({ source = "unblevable/quick-scope" })

  vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#afff5f", underline = true })
  vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "#5fffff", underline = true })
end)
