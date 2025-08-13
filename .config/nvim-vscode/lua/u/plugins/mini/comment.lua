local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({
    source = "JoosepAlviste/nvim-ts-context-commentstring",
  })
  require("ts_context_commentstring").setup({
    enable_autocmd = false,
  })
  require("mini.comment").setup({
    options = {
      ignore_blank_line = true,
      start_of_line = true,
      custom_commentstring = function()
        return require("ts_context_commentstring").calculate_commentstring() or vim.bo.commentstring
      end,
    },
  })
end)
