---@type LazySpec
return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  ---@type snacks.Config
  opts = {
    bigfile = { enabled = true },
    indent = { enabled = true },
    input = { enabled = true },
    notifier = { enabled = true },
    statuscolumn = {
      enabled = true,
      folds = { open = true },
    },
  },
  keys = {
    {
      "<C-w>m",
      function()
        require("snacks").zen.zoom()
      end,
      desc = "Toggle Zoom",
    },
    {
      "<leader>uN",
      function()
        require("snacks").notifier.hide()
      end,
      desc = "Dismiss All Notifications",
    },
    {
      "<leader>un",
      function()
        require("snacks").notifier.show_history()
      end,
      desc = "Notification History",
    },
    {
      "<leader>bd",
      function()
        require("snacks").bufdelete.delete()
      end,
      desc = "Delete",
    },
    {
      "<leader>bD",
      function()
        require("snacks").bufdelete.all()
      end,
      desc = "Delete All",
    },
    {
      "<leader>bo",
      function()
        require("snacks").bufdelete.other()
      end,
      desc = "Delete Others",
    },
  },
  init = function()
    vim.api.nvim_create_autocmd("User", {
      pattern = "VeryLazy",
      callback = function()
        Snacks.toggle.inlay_hints():map "<leader>uh"
        Snacks.toggle.indent():map "<leader>ui"
        Snacks.toggle.treesitter():map "<leader>uT"
        Snacks.toggle.diagnostics():map "<leader>ud"
      end,
    })
  end,
}
