---@type LazySpec
return {
  {
    "mikavilpas/yazi.nvim",
    enabled = true,
    dependencies = { "folke/snacks.nvim" },
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
  {
    "stevearc/oil.nvim",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {
      default_file_explorer = false,
      win_options = { wrap = true },
      lsp_file_methods = { timeout_ms = 2000, autosave_changes = true },
      delete_to_trash = true,
      kip_confirm_for_simple_edits = true,
      watch_for_changes = false,
    },
    dependencies = { "nvim-mini/mini.icons" },
    cmd = "Oil",
  },
}
