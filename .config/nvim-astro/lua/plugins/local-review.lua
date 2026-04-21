---@type LazySpec
return {
  "ssundarraj/local-review.nvim",
  keys = {
    { "<Leader>ca", desc = "Review: Add" },
    { "<Leader>cd", desc = "Review: Delete" },
    { "<Leader>ce", desc = "Review: Export" },
    { "]r", desc = "Review: Next" },
    { "[r", desc = "Review: Prev" },
  },
  opts = {
    marker_text = "●",
    marker_hl = "DiagnosticHint",
    keymaps = {
      comment = "<leader>ca",
      delete = "<leader>cd",
      next = "]r",
      prev = "[r",
      export = "<leader>ce",
    },
    comment_close_keys = {
      { modes = { "n" }, key = "q" },
      { modes = { "n", "i" }, key = "<C-c>" },
    },
  },
}
