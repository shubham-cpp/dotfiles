---@type LazySPec
return {
  {
    "simonmclean/triptych.nvim",
    enabled = false,
    dependencies = {
      "nvim-lua/plenary.nvim", -- required
      "echasnovski/mini.icons", -- optional for icons
      "antosha417/nvim-lsp-file-operations", -- optional LSP integration
    },
    opts = {
      mappings = {
        open_hsplit = { "-", "<c-x>" },
        open_vsplit = { "|", "<c-v>" },
        toggle_hidden = { "g.", "gh" },
      },
      options = {
        line_numbers = { enabled = true, relative = true },
      },
      extension_mappings = {
        ["<c-f>"] = {
          mode = "n",
          fn = function(target, _)
            require("snacks.picker").grep({
              dirs = { target.path },
            })
          end,
        },
        ["<c-p>"] = {
          mode = "n",
          fn = function(target, _) require("snacks.picker").files({ cwd = target.dirname }) end,
        },
      },
    }, -- config options here
    keys = {
      { "<leader>e", "<cmd>Triptych<CR>", desc = "Toggle Triptych" },
    },
  },
  {
    "mikavilpas/yazi.nvim",
    enabled = true,
    dependencies = { "snacks.nvim" },
    ---@type YaziConfig | {}
    opts = {
      open_for_directories = false,
    },
    keys = {
      {
        "<leader>-",
        mode = { "n", "v" },
        "<cmd>Yazi<cr>",
        desc = "Open yazi at the current file",
      },
      {

        "<leader>0",
        "<cmd>Yazi cwd<cr>",
        desc = "Open the file manager in nvim's working directory",
      },
    },
  },
}
