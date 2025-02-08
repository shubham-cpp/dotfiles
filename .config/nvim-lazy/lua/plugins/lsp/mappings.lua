---@type LazySpec
return {
  "neovim/nvim-lspconfig",
  opts = function()
    local keys = require("lazyvim.plugins.lsp.keymaps").get()
    if LazyVim.pick.want() == "fzf" then
      keys[#keys + 1] = { "<leader>cw", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document symbols" }
      keys[#keys + 1] = { "<leader>cW", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", desc = "Workspace symbols" }
    elseif LazyVim.pick.want() == "telescope" then
      keys[#keys + 1] = { "<leader>cw", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Document symbols" }
      keys[#keys + 1] = { "<leader>cW", "<cmd>Telescope lsp_workspace_symbols<cr>", desc = "Workspace symbols" }
    elseif LazyVim.pick.want() == "snacks" then
      keys[#keys + 1] = {
        "<leader>cw",
        function()
          Snacks.picker.lsp_symbols()
        end,
        desc = "Document symbols",
      }
      keys[#keys + 1] = {
        "<leader>cW",
        function()
          Snacks.picker.lsp_workspace_symbols()
        end,
        desc = "Workspace symbols",
      }
    else
      keys[#keys + 1] = { "<leader>cw", vim.lsp.buf.document_symbol, desc = "Document symbols" }
      keys[#keys + 1] = { "<leader>cW", vim.lsp.buf.workspace_symbol, desc = "Workspace symbols" }
    end
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
