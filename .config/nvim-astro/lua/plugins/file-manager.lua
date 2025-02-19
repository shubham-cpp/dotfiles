return {
  "mikavilpas/yazi.nvim",
  enabled = true,
  dependencies = { "snacks.nvim" },
  ---@type YaziConfig | {}
  opts = {
    open_for_directories = false,
  },
  keys = {
    {
      "<leader>e",
      mode = { "n", "v" },
      "<cmd>Yazi<cr>",
      desc = "Open yazi at the current file",
    },
    {
      "<leader>E",
      "<cmd>Yazi cwd<cr>",
      desc = "Open the file manager in nvim's working directory",
    },
    {
      "<c-/>",
      "<cmd>Yazi toggle<cr>",
      desc = "Resume the last yazi session",
    },
  },
}
