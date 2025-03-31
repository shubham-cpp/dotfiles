---@type LazySpec
return {
  {
    "numToStr/Comment.nvim",
    dependencies = { "JoosepAlviste/nvim-ts-context-commentstring" },
    event = "BufWinEnter",
    config = function()
      require("ts_context_commentstring").setup({
        enable_autocmd = false,
      })
      require("Comment").setup({
        ignore = "^$",
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
      })
    end,
  },
  -- {
  --   'JoosepAlviste/nvim-ts-context-commentstring',
  --   lazy = true,
  --   opts = { enable_autocmd = false, }
  -- }
}
