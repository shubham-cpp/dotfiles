---@param picker snacks.Picker
local function copy_path_full(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then
    return
  end
  vim.schedule(function()
    local full_path = vim.fn.fnamemodify(selected.file, ":p")
    vim.fn.setreg(vim.v.register, full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = "File Path Copied" })
  end)
end
---@type LazySpec
return {
  "folke/snacks.nvim",
  optional = true,
  ---@type snacks.Config
  opts = {
    dashboard = { enabled = false },
    picker = {
      enabled = true,
      ui_select = true,
      layout = { preset = "dropdown" },
      matcher = { frecency = true, history_bonus = true },
      formatters = { file = { filename_first = true } },
      sources = {
        explorer = {
          layout = { cycle = false, layout = { position = "right" } },
          actions = { copy_path_full = copy_path_full },
          win = {
            list = {
              keys = {
                ["/"] = false,
                ["f"] = { "toggle_focus" },
                ["Y"] = { "copy_path_full" },
                ["gf"] = { "picker_files", desc = "Open File Picker" },
                ["<leader>f"] = { "picker_files", desc = "Open File Picker" },
              },
            },
          },
        },
      },
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
      "<C-p>",
      function()
        local branch = vim.b.gitsigns_head or nil
        if branch ~= nil and branch ~= "" then
          Snacks.picker.git_files({ layout = { preset = "vscode" }, untracked = true })
        else
          Snacks.picker.files({ layout = { preset = "vscode" } })
        end
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
        Snacks.picker.git_files({ untracked = true, layout = { preset = "vscode" } })
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
    {
      "<leader>uN",
      function()
        Snacks.notifier.show_history()
      end,
      desc = "Notification History",
    },
    {
      "<c-w>m",
      function()
        Snacks.zen.zoom()
      end,
      desc = "Toggle Zoom",
    },
    {
      "<C-\\>",
      function()
        Snacks.terminal(nil, { win = { style = "float", border = "rounded" } })
      end,
      desc = "Toggle terminal",
    },
    {
      "<C-\\>",
      "<cmd>close<cr>",
      mode = "t",
      desc = "Hide terminal",
    },
  },
}
