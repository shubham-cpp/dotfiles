---@type LazySpec
return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    picker = {
      enabled = true,
      ui_select = true,
      layout = { preset = "dropdown" },
      matcher = { frecency = true },
      formatters = { file = { filename_first = true } },
      -- jump = { reuse_win = true },
      win = {
        -- input window
        input = {
          keys = {
            ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
            ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
            ["<c-c>"] = { "copy", mode = { "i", "n" } },
          },
        },
        list = { keys = { ["<c-x>"] = "edit_split" } },
      },
    },
  },
  keys = {
    {
      "<leader>uN",
      function()
        Snacks.notifier.show_history()
      end,
      desc = "Notification History",
    },
    {
      "<C-Y>",
      function()
        Snacks.terminal({ "yazi" }, { cwd = vim.uv.cwd() })
      end,
      desc = "Toggle Yazi",
      mode = { "n", "t" },
    },
    {
      "<C-p>",
      function()
        Snacks.picker.files({ layout = { preset = "vscode" } })
      end,
      desc = "Find Files",
    },
    {
      "<leader>ff",
      function()
        Snacks.picker.files({ layout = { preset = "vscode" } })
      end,
      desc = "Find Files",
    },
    {
      "<leader>fg",
      function()
        Snacks.picker.git_files({ layout = { preset = "vscode" } })
      end,
      desc = "Find Files(git)",
    },
    {
      "<leader>fn",
      function()
        Snacks.picker.files({ cwd = vim.fn.stdpath("config"), layout = { preset = "vscode" } })
      end,
      desc = "Find Config File",
    },
    {
      "<leader>fN",
      function()
        Snacks.picker.files({ cwd = vim.fn.stdpath("data") .. "/lazy", layout = { preset = "vscode" } })
      end,
      desc = "Neovim Data dir",
    },
    {
      "<leader>fd",
      function()
        Snacks.picker.files({ cwd = vim.fn.expand("~/Documents/dotfiles/.config"), layout = { preset = "vscode" } })
      end,
      desc = "Find Dotfiles",
    },
    {
      "<leader>fz",
      function()
        Snacks.picker.zoxide()
      end,
      desc = "Zoxided",
    },
  },

  -- init = function()
  --   vim.api.nvim_create_user_command("NotificationHistory", function()
  --     if not _G.Snacks then
  --       return
  --     end
  --     Snacks.notifier.show_history()
  --   end, { desc = "Show Notification History" })
  -- end,
}
