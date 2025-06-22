---@type LazySpec
return {
  "axelvc/template-string.nvim",
  event = "InsertEnter",
  ft = { "html", "typescript", "javascript", "typescriptreact", "javascriptreact", "vue", "svelte", "python", "cs","astro" },
  dependencies = { "nvim-treesitter/nvim-treesitter", },
  opts = {},
}
