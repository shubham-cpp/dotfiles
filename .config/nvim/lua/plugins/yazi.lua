return {
  url = "mikavilpas/yazi.nvim",
  config = function()
    require("yazi").setup({
      open_for_directories = false,
      integrations = {
        grep_in_directory = "fzf-lua",
        grep_in_selected_files = "fzf-lua",
      },
    })
  end,
  keys = {
    { "<leader>-", "<cmd>Yazi<cr>", mode = { "n", "v" }, desc = "open at current file" },
    { "<leader>_", "<cmd>Yazi cwd<cr>", desc = "open at working directory" },
  },
}
