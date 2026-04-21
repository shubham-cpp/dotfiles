return {
  {
    "jellydn/tiny-term.nvim",
    keys = {
      { "<C-\\>", function()
        if vim.v.count ~= 0 then
          vim.g.previous_term_count = vim.v.count1
        end
        require("tiny-term").toggle(nil, {
          win = { position = "float", border = "rounded" },
          count = vim.g.previous_term_count,
        })
      end, mode = { "n", "t" }, desc = "Toggle terminal with count" },
    },
    opts = { win = { position = "float", border = "rounded" } },
  },
  {
    "akinsho/toggleterm.nvim",
    enabled = false,
  },
}
