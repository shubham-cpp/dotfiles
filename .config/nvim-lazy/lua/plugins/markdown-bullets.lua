local fts = { "markdown", "gitcommit", "text", "Avante" }
---@type LazySpec
return {
  "bullets-vim/bullets.vim",
  ft = fts,
  init = function()
    vim.g.bullets_enabled_file_types = fts
  end,
}
