---@type LazySpec
return {
  "axelvc/template-string.nvim",
  -- event = "InsertEnter",
  ft = { "html", "typescript", "javascript", "typescriptreact", "javascriptreact", "vue", "svelte", "python", "cs" }, -- filetypes where the plugin is active
  dependencies = { "nvim-treesitter" },
  opts = {},
}
