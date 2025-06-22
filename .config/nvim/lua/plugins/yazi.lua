---@type LazySpec
return {
  {
    "mikavilpas/yazi.nvim",
    enabled = true,
    -- dependencies = { "folke/snacks.nvim" },
    ---@type YaziConfig | {}
    opts = {
      open_for_directories = true,
    },
    cmd = "Yazi",
    keys = {
      {
        "<leader>-",
        mode = { "n", "v" },
        "<cmd>Yazi<cr>",
        desc = "Yazi: current file",
      },
      {
        "<leader>e",
        mode = { "n", "v" },
        "<cmd>Yazi<cr>",
        desc = "Yazi: current file",
      },
      {
        "<leader>_",
        "<cmd>Yazi cwd<cr>",
        desc = "Yazi: cwd",
      },
      {
        "<leader>E",
        "<cmd>Yazi cwd<cr>",
        desc = "Yazi: cwd",
      },
    },
  },
}
