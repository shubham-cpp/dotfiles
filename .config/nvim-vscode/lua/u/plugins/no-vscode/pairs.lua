local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "windwp/nvim-autopairs" })
  require("nvim-autopairs").setup({
    enable_check_bracket_line = false,
    fast_wrap = {},
  })
end)
