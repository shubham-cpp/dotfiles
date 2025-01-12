---@type LazySpec
return {
  "mikavilpas/yazi.nvim",
  version = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  cmd = "Yazi",
  keys = {
    { "<C-y>", "<cmd>Yazi toggle<cr>", desc = "Togggle Yazi" },
    { "<leader>-", "<cmd>Yazi<cr>", desc = "Open Yazi at the current file" },
    { "<leader>tf", "<cmd>Yazi cwd<cr>", desc = "File Manager in cwd(Yazi)" },
  },
  ---@type YaziConfig
  opts = {
    open_for_directories = false,
    use_ya_for_events_reading = true,
    use_yazi_client_id_flag = true,
    highlight_hovered_buffers_in_same_directory = false,
    integrations = {
      --- What should be done when the user wants to grep in a directory
      ----@param directory string
      -- grep_in_directory = function(directory)
      --   require("telescope.builtin").live_grep({
      --     prompt = "Search in " .. directory,
      --     cwd = directory,
      --   })
      -- end,
    },
  },
}
