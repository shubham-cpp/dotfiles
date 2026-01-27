---@type LazySpec
return {
  {
    "williamboman/mason.nvim",
    optional = true,
    opts = {
      ---@type '"prepend"' | '"append"' | '"skip"'
      PATH = "prepend",
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = require("astrocore").list_insert_unique(
        opts.ensure_installed,
        { "css_variables", "cssmodules_ls", "emmet_language_server", "django-template-lsp" }
      )
      opts.ensure_installed = vim.tbl_filter(
        function(server) return server ~= "emmet_ls" end,
        opts.ensure_installed or {}
      )
    end,
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = require("astrocore").list_insert_unique(opts.ensure_installed, {
        "emmet-language-server",
        "cssmodules-language-server",
        "css-variables-language-server",
        "django-template-lsp",
      })
      opts.ensure_installed = vim.tbl_filter(
        function(server) return server ~= "emmet-ls" end,
        opts.ensure_installed or {}
      )
    end,
  },
}
