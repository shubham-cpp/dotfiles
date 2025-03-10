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
  {
    "jay-babu/mason-nvim-dap.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = vim.tbl_filter(function(value) return value ~= "haskell" end, opts.ensure_installed)
      -- require("astrocore").list_insert_unique(opts.ensure_installed, { "haskell" })
    end,
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = vim.tbl_filter(
        function(value) return value ~= "haskell-debug-adapter" or value ~= "haskell-language-server" end,
        opts.ensure_installed
      )
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = vim.tbl_filter(function(value) return value ~= "hls" end, opts.ensure_installed)
    end,
  },
}
