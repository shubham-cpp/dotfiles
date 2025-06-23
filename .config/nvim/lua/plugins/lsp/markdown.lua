local fts = { "markdown", "gitcommit", "text", "Avante" }

---@type LazySpec
return {
  {
    "bullets-vim/bullets.vim",
    ft = fts,
    enabled = true,
    init = function()
      vim.g.bullets_enabled_file_types = fts
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = {
      ensure_installed = { "markdown", "markdown_inline" },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "marksman" } },
  },
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers["marksman"] = {}
    end,
  },
}
