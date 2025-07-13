---@type LazySpec
return {
  { "tpope/vim-repeat", enabled = false, event = "VeryLazy" },
  {
    "tpope/vim-abolish",
    keys = { "cr" },
    cmd = { "Abolish", "Subvert", "S" },
  },
  {
    "tpope/vim-dispatch",
    enabled = true,
    cmd = { "Dispatch", "Make", "Focus", "Start", "FocusDispatch", "Spawn", "Copen", "AbortDispatch" },
  },
}
