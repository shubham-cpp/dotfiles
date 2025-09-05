---@type LazySpec
return {
  "stevearc/oil.nvim",
  enabled = vim.g.vscode == nil,
  opts = {
    columns = { "icon", "size" },
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
    { "-", require("l.config.utils").cmd_str "Oil --float", desc = "Open parent directory" },
  },
}
