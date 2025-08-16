local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "tpope/vim-commentary" })
  add({ source = "JoosepAlviste/nvim-ts-context-commentstring" })
  vim.keymap.set(
    'n',
    'gc',
    '<Plug>ContextCommentaryLine', -- Previously '<Plug>CommentaryLine'
    { silent = true, desc = 'Comment line' }
  )
end)
