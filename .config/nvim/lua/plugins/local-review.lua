return {
  url = "ssundarraj/local-review.nvim",
  keys = {
    { "<leader>ca", desc = "Review: Add" },
    { "<leader>cd", desc = "Review: Delete" },
    { "<leader>ce", desc = "Review: Export" },
    { "]r", desc = "Review: Next" },
    { "[r", desc = "Review: Prev" },
  },
  config = function()
    require("local_review").setup({
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
    })
  end,
}
