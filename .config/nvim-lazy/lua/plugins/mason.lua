return {
  {
    "mason-org/mason.nvim",
    version = "^1.0.0",
    optional = true,
    opts = {
      ---@type '"prepend"' | '"append"' | '"skip"'
      PATH = "append",
      ui = {
        icons = { package_pending = " ", package_installed = " ", package_uninstalled = " " },
      },
      ensure_installed = {
        "goimports",
        "gofumpt",
        "golangci-lint",
        "html-lsp",
        "css-lsp",
        "eslint_d",
        "cssmodules-language-server",
        "css-variables-language-server",
        "emmet-language-server",
      },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    version = "^1.0.0",
    optional = true,
  },
}
