---@type LazySpec
return {
  {
    "mikavilpas/yazi.nvim",
    enabled = true,
    dependencies = { "folke/snacks.nvim" },
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
        "<leader>_",
        "<cmd>Yazi cwd<cr>",
        desc = "Resume the last yazi session",
      },
    },
  },
}
