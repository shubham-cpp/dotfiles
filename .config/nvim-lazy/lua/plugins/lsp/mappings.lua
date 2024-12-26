---@type LazySpec
return {
  "neovim/nvim-lspconfig",
  opts = function()
    local keys = require("lazyvim.plugins.lsp.keymaps").get()
    -- change a keymap
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
    -- add a keymap
    -- keys[#keys + 1] = { "H", "<cmd>echo 'hello'<cr>" }
  end,
}
