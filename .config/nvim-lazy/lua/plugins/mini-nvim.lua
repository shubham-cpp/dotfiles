---@type LazySpec
return {
  {
    "echasnovski/mini.align",
    version = "*",
    keys = { { "ga", mode = { "n", "x" } }, { "gA", mode = { "n", "x" } } },
    opts = {},
  },
  {
    "echasnovski/mini.move",
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
    "echasnovski/mini.operators",
    version = "*",
    enabled = true,
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gs", mode = { "n", "x" }, desc = "Sort" },
      "X",
    },
    opts = {
      -- Exchange text regions
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
  {
    "echasnovski/mini.files",
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
}
