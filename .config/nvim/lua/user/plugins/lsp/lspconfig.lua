---@type LazySpec
return {
  "neovim/nvim-lspconfig",
  event = "BufWinEnter",
  dependencies = {
    { "iguanacucumber/mag-nvim-lsp", name = "cmp-nvim-lsp", optional = true },
    {
      "SmiteshP/nvim-navic",
      opts = { lazy_update_context = true },
    },
    {
      "AstroNvim/astrolsp",
      opts = require "user.config.astrolsp",
    },
    {
      "williamboman/mason-lspconfig.nvim",
      -- opts_extend ={'ensure_installed'},
      dependencies = { "mason.nvim" },
      opts = function()
        return {
          -- ensure_installed = {'efm'},
          -- use AstroLSP setup for mason-lspconfig
          handlers = { function(server) require("astrolsp").lsp_setup(server) end },
        }
      end,
    },
  },
  config = function()
    -- set up servers configured with AstroLSP
    vim.tbl_map(require("astrolsp").lsp_setup, require("astrolsp").config.servers)
  end,
}
