local fts = { "markdown", "gitcommit", "text", "Avante" }
---@type LazySpec
return {
  "bullets-vim/bullets.vim",
  ft = fts,
  enabled = true,
  init = function() vim.g.bullets_enabled_file_types = fts end,
}
