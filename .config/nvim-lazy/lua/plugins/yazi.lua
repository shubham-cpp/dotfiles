---@type LazySpec
return {
  "mikavilpas/yazi.nvim",
  ---@type YaziConfig | {}
  opts = {
    open_for_directories = false,
    integrations = {
      grep_in_directory = "fzf-lua",
      grep_in_selected_files = "fzf-lua",
    },
  },
  keys = {
    {
      "<leader>-",
      "<cmd>Yazi<cr>",
      mode = { "n", "v" },
      desc = "Open yazi at the current file",
    },
    {
      "<leader>_",
      "<cmd>Yazi cwd<cr>",
      desc = "Resume the last yazi session",
    },
  },
}
