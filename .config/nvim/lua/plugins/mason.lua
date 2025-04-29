---@type LazySpec
return {
  -- use mason-lspconfig to configure LSP installations
  {
    "mason.nvim",
    optional = true,
    opts = {
      ---@type '"prepend"' | '"append"' | '"skip"'
      PATH = "append",
      ui = {
        icons = {
          package_pending = " ",
          package_installed = " ",
          package_uninstalled = " ",
        },
      },
    },
  },
}
