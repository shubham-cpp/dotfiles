---@type LazySpec
return {
  {
    "neovim/nvim-lspconfig",
    opts = function()
      local keys = require("lazyvim.plugins.lsp.keymaps").get()
      keys[#keys + 1] = { "gl", vim.diagnostic.open_float, desc = "Open diagnostic" }
      -- disable a keymap
      keys[#keys + 1] = { "<C-k>", mode = "i", false }
      keys[#keys + 1] = {
        "<C-h>",
        function()
          return vim.lsp.buf.signature_help()
        end,
        mode = "i",
        desc = "Signature Help",
        has = "signatureHelp",
      }
    end,
  },
  {
    "williamboman/mason.nvim",
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
        "cssmodules-language-server",
        "css-variables-language-server",
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        go = { "golangcilint" },
      },
    },
  },
}
