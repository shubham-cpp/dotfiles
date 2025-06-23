---@type LazySpec
return {
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "tailwindcss" }, },
  },
  {
    'neovim/nvim-lspconfig',
    opts = function(_,opts)
      opts.servers['tailwindcss'] = {}
    end
  }
}
