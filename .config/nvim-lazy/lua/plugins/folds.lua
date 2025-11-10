---@type LazySpec
return {
  {
    "kevinhwang91/nvim-ufo",
    dependencies = "kevinhwang91/promise-async",
    event = "LspAttach",
    opts = {},
    keys = function()
      local ufo = require("ufo")
      return {
        { "zR", ufo.openAllFolds },
        { "zM", ufo.closeAllFolds },
        { "zm", ufo.closeFoldsWith },
      }
    end,
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        ["*"] = {
          capabilities = {
            textDocument = {
              foldingRange = {
                dynamicRegistration = false,
                lineFoldingOnly = true,
              },
            },
          },
        },
      },
    },
  },
}
