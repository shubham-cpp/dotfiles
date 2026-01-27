return {
  {
    "mason-org/mason.nvim",
    optional = true,
    opts = {
      ---@type '"prepend"' | '"append"' | '"skip"'
      PATH = "prepend",
      ui = {
        icons = { package_pending = " ", package_installed = " ", package_uninstalled = " " },
      },
      ensure_installed = {
        "emmylua_ls",
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
}
