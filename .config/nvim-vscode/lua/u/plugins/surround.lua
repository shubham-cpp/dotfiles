local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "kylechui/nvim-surround", })
  require("nvim-surround").setup({})
end)
