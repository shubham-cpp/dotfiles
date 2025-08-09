---@type LazySpec
return {
  'stevearc/oil.nvim',
  enabled = vim.g.vscode == nil,
  opts = {
    columns = {
      "icon",
      -- "permissions",
      "size",
      -- "mtime",
    },
    watch_for_changes = true,
    delete_to_trash = true,
    skip_confirm_for_simple_edits = true,
    lsp_file_methods = {
      enabled = true,
      timeout_ms = 2000,
      autosave_changes = true,
    },
    keymaps = {
      Q = { "actions.close", mode = "n" },
    },
  },
  keys = {
    {"-", "<cmd>Oil --float<cr>",  desc = "Open parent directory" }
  }
}
