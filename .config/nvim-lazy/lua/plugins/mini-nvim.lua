---@type LazySpec
return {
  {
    "nvim-mini/mini.surround",
    version = "*",
    keys = { { "sa", mode = { "n", "x" } }, { "sr", mode = { "n", "x" } }, { "sd", mode = { "n", "x" } } },
    opts = {},
  },
  {
    "nvim-mini/mini.align",
    version = "*",
    keys = { { "ga", mode = { "n", "x" } }, { "gA", mode = { "n", "x" } } },
    opts = {},
  },
  {
    "nvim-mini/mini.move",
    keys = {
      { "<", mode = "v" },
      { "J", mode = "v" },
      { "K", mode = "v" },
      { ">", mode = "v" },
      "<M-h>",
      "<M-j>",
      "<M-k>",
      "<M-l>",
    },
    opts = {
      -- Move current line in Visual mode
      mappings = {
        left = "<",
        right = ">",
        down = "J",
        up = "K",

        -- Move current line in Normal mode
        line_left = "<M-h>",
        line_right = "<M-l>",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
  },
  {
    "nvim-mini/mini.operators",
    version = "*",
    enabled = true,
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gS", mode = { "n", "x" }, desc = "Sort" },
      { "X", "x$", desc = "Replace to end of line", remap = true },
    },
    opts = {
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
      sort = { prefix = "gS" },
    },
  },
  {
    "nvim-mini/mini.files",
    optional = true,
    keys = {
      {
        "<leader>e",
        function()
          if not require("mini.files").close() then
            require("mini.files").open(vim.api.nvim_buf_get_name(0))
          end
        end,
        desc = "Open mini.files (Directory of Current File)",
      },
      {
        "<leader>E",
        function()
          if not require("mini.files").close() then
            require("mini.files").open(vim.uv.cwd(), true)
          end
        end,
        desc = "Open mini.files (cwd)",
      },
    },
    opts = {
      options = {
        permanent_delete = false,
      },
      --- More mapping are in autocmds file
      mappings = {
        go_in = "L",
        go_in_plus = "l",
        go_in_horizontal = "<C-w>S",
        go_in_horizontal_plus = "<C-w>s",
        go_in_vertical = "<C-w>V",
        go_in_vertical_plus = "<C-w>v",
      },
    },
    dependencies = {
      {
        "s1n7ax/nvim-window-picker",
        name = "window-picker",
        lazy = true,
        version = "2.*",
        opts = {
          -- hint = "floating-big-letter",
          -- selection_chars = 'FJDKSLA;CMRUEIWOQP',
          selection_chars = "1234567890",
          picker_config = {
            handle_mouse_click = true,
            statusline_winbar_picker = {
              selection_display = function(char)
                return "%=" .. "%#Underlined#" .. char .. "%*" .. string.rep(" ", 16)
              end,
            },
          },
          highlights = {
            enabled = true,
            winbar = {
              focused = {
                fg = "#fefefe",
                bg = "#252530",
                bold = true,
              },
              unfocused = {
                fg = "#fefefe",
                bg = "#252530",
                bold = true,
              },
            },
            statusline = {
              focused = {
                fg = "#fefefe",
                bg = "#252530",
                bold = true,
              },
              unfocused = {
                fg = "#fefefe",
                bg = "#252530",
                bold = true,
              },
            },
          },
        },
      },
    },
  },
  {
    "nvim-mini/mini.visits",
    opts = {},
    keys = function()
      local visit_marks = require("config.visit_marks")
      visit_marks.setup({})

      local ks = {
        { "<Leader>va", visit_marks.toggle, desc = "Toggle File" },
        { "<Leader>vv", visit_marks.toggle_window, desc = "Toggle List" },
        { "<Leader>vj", visit_marks.jump_input, desc = "Jump Index" },
      }

      for index = 1, 9 do
        table.insert(ks, {
          "<Leader>v" .. index,
          function()
            visit_marks.jump(index)
          end,
          desc = "Jump " .. index,
        })
        table.insert(ks, {
          "<LocalLeader>" .. index,
          function()
            visit_marks.jump(index)
          end,
          desc = "Jump " .. index,
        })
      end

      return ks
    end,
  },
}
