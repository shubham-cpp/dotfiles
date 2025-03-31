local fts = { "markdown", "vimwiki", "Avante" }
local bullets_fts = { "markdown", "gitcommit", "text", "Avante" }
---@type LazySpec
return {
  {
    "bullets-vim/bullets.vim",
    ft = bullets_fts,
    init = function() vim.g.bullets_enabled_file_types = bullets_fts end,
  },
  {
    "MeanderingProgrammer/render-markdown.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter", "echasnovski/mini.icons" },
    ft = fts,
    ---@module 'render-markdown'
    ---@type render.md.UserConfig
    opts = {
      file_types = fts,
    },
  },
}
