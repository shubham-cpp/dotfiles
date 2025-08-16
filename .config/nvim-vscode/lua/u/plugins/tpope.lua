local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "tpope/vim-repeat" })
  add({ source = "tpope/vim-abolish" })
  add({ source = "tpope/vim-dispatch" })
end)
