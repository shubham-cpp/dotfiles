return {
  {
    "mikavilpas/yazi.nvim",
    enabled = true,
    dependencies = { "snacks.nvim" },
    ---@type YaziConfig | {}
    opts = {
      open_for_directories = false,
    },
    keys = {
      {
        "<leader>-",
        mode = { "n", "v" },
        "<cmd>Yazi<cr>",
        desc = "Open yazi at the current file",
      },
      {
        "<c-/>",
        "<cmd>Yazi cwd<cr>",
        desc = "Resume the last yazi session",
      },
    },
  },
}
