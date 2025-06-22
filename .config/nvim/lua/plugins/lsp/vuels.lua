---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "vue" } },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "volar" }, },
  },
  {
    'neovim/nvim-lspconfig',
    opts = function(_, opts)
      opts.servers.volar = {
        init_options = {
          vue = { hybridMode = true, },
        },
      }
    end
  }
}
