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
    statuscolumn = { enabled = true, folds = { open = true } },
    picker = {
      enabled = true,
      layout = {
        preset = "dropdown",
      },
      ---@class snacks.picker.matcher.Config
      matcher = { frecency = true, history_bonus = true },
      formatters = { file = { filename_first = true } },
      win = {
        -- input window
        input = {
          keys = {
            ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
          },
        },
        list = {
          keys = {
            ["<c-x>"] = "edit_split",
          },
        },
      },
      sources = {
        buffers = {
          win = {
            input = {
              keys = {
                ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
                ["<a-x>"] = { "bufdelete", mode = { "n", "i" } },
              },
            },
            list = { keys = { ["dd"] = "bufdelete" } },
          },
        },
        git_files = { untracked = true },
        git_grep = { untracked = true },
      },
    },
  },
  -- stylua: ignore
  keys = {
    { "<leader><space>", function() Snacks.picker.smart() end,                                      desc = "Smart Find Files" },
    { "<leader>,",       function() Snacks.picker.buffers() end,                                    desc = "Buffers"          },
    { "<leader>/",       function() Snacks.picker.grep() end,                                       desc = "Grep"             },
    { "<leader>:",       function() Snacks.picker.command_history() end,                            desc = "Command History"  },
    { "<leader>fN",      function() Snacks.picker.notifications() end,                              desc = "Notifications"    },
    { "<leader>fb",      function() Snacks.picker.buffers() end,                                    desc = "Buffers"          },
    { "<leader>fB",      function() Snacks.picker.grep_buffers() end,                               desc = "Grep Buffers"     },
    { "<leader>ff",      function() Snacks.picker.files { layout = { preset = "vscode" } } end,     desc = "Find Files"       },
    { "<leader>fg",      function() Snacks.picker.git_files { layout = { preset = "vscode" } } end, desc = "Find Git Files"   },
    { "<leader>fo",      function() Snacks.picker.recent() end,                                     desc = "Recent"           },
    { "<leader>fh",      function() Snacks.picker.help() end,                                       desc = "Help Pages"       },
    { "<leader>fi",      function() Snacks.picker.icons() end,                                      desc = "Icons"            },
    { "<leader>fk",      function() Snacks.picker.keymaps() end,                                    desc = "Keymaps"          },
    { "<leader>fm",      function() Snacks.picker.marks() end,                                      desc = "Marks"            },
    { "<leader>fM",      function() Snacks.picker.man() end,                                        desc = "Man Pages"        },
    { "<leader>fl",      function() Snacks.picker.lazy() end,                                       desc = "Lazy"             },
    { "<leader>fq",      function() Snacks.picker.qflist() end,                                     desc = "Quickfix List"    },
    { "<leader>fr",      function() Snacks.picker.resume() end,                                     desc = "Resume"           },
    { "<leader>fu",      function() Snacks.picker.undo() end,                                       desc = "Undo History"     },
    { "<leader>fz",      function() Snacks.picker.zoxide() end,                                     desc = "Zoxide"           },
    { "<leader>fs",      function() Snacks.picker.grep() end,                                       desc = "Grep"             },
    { "<leader>fw",      function() Snacks.picker.grep_word() end,                                  desc = "Grep word", mode = { "n", "x" } },

    { "<leader>fn",      function()
      Snacks.picker.files { cwd = vim.fn.stdpath "config", layout = { preset = "vscode" } }
    end, desc = "Find Config File" },
    { "<leader>fd",      function()
      Snacks.picker.git_files { cwd = vim.fn.expand "~/Documents/dotfiles" , layout = { preset = "vscode" } }
    end, desc = "Find dotfiles" },
    --- git
    --- stylua: ignore
    { "<leader>gb", function() Snacks.picker.git_branches() end, desc = "Git Branches" },
    { "<leader>gc", function() Snacks.picker.git_log() end,      desc = "Git Log" },
    { "<leader>gC", function() Snacks.picker.git_log_line() end, desc = "Git Log Line" },
    { "<leader>gs", function() Snacks.picker.git_status() end,   desc = "Git Status" },
    { "<leader>gS", function() Snacks.picker.git_stash() end,    desc = "Git Stash" },
    { "<leader>gD", function() Snacks.picker.git_diff() end,     desc = "Git Diff (Hunks)" },
    { "<leader>gf", function() Snacks.picker.git_log_file() end, desc = "Git Log File" },

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
