---@type LazySpec
return {
  {
    "mason-org/mason-lspconfig.nvim",
    opts = {
      ensure_installed = {
        "html",
        "cssls",
        "cssmodules_ls",
        "css_variables",
        "emmet_language_server",
        "astro",
        "prismals",
        "taplo"
      },
    },
  },
  {
    "mason-org/mason.nvim",
    optional = true,
    opts = { ensure_installed = { "htmlhint" } },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "html", "css", "scss", "styled", "astro", "prisma","toml" } },
  },
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers["html"] = { init_options = { provideFormatter = false } }
      opts.servers["cssls"] = { init_options = { provideFormatter = false } }
      opts.servers["css_variables"] = {}
      opts.servers["cssmodules_ls"] = {}
      opts.servers["emmet_language_server"] = {}
      opts.servers["astro"] = {}
      opts.servers["prismals"] = {}
      opts.servers["taplo"] = {}

      vim.treesitter.language.register("scss", "less")
      vim.treesitter.language.register("scss", "postcss")
    end,
  },
}
