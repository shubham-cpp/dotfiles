---@type LazySpec
return {
  {
    "mason-org/mason.nvim",
    version = "^1.0.0",
    cmd = "Mason",
    build = ":MasonUpdate",
    opts = {
      PATH = "append",
      ui = {
        icons = {
          package_pending = " ",
          package_installed = " ",
          package_uninstalled = " ",
        },
      },
      ensure_installed = {
        "stylua",
        "shfmt",
      },
    },
    opts_extend = { "ensure_installed" },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    version = "^1.0.0",
    dependencies = "mason-org/mason.nvim",
    opts_extend = { "ensure_installed" },
    opts = {
      ensure_installed = { "lua_ls" },
    },
  },
}
