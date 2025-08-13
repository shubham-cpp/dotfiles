local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "stevearc/oil.nvim" })
  require("oil").setup({
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
  })
  vim.keymap.set("n", "-", require("u.utils").cmd_str "Oil --float", { desc = "Open parent directory" })
end)
