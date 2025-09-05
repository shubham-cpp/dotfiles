---@type LazySpec
return {
  { "tpope/vim-repeat", event = "VeryLazy" },
  { "tpope/vim-abolish", event = "VeryLazy" },
  { "tpope/vim-dispatch", event = "VeryLazy" },
  {
    "tpope/vim-commentary",
    event = "VeryLazy",
    dependencies = "JoosepAlviste/nvim-ts-context-commentstring",
    keys = {
      { "gc", "<Plug>ContextCommentaryLine", silent = true, desc = "Comment line" },
    },
  },
}
