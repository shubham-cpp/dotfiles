return {
  {
    url = "nvim-lualine/lualine.nvim",
    config = function()
      local diag_icons = require("core.icons")

      require("lualine").setup({
        options = {
          theme = "auto",
          icons_enabled = true,
        },
        sections = {
          lualine_a = { {
            "mode",
            fmt = function(str)
              return str:sub(1, 1)
            end,
          } },
          lualine_b = {
            "branch",
            "diff",
            { "diagnostics", symbols = diag_icons },
          },
          lualine_c = { "filename", { "navic", color_correction = "static" } },
          lualine_x = { "filetype" },
          lualine_y = { "progress" },
          lualine_z = { "location" },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = { "branch" },
          lualine_c = { "filename" },
          lualine_x = {},
          lualine_y = {},
          lualine_z = {},
        },
      })
    end,
  },

  {
    url = "folke/which-key.nvim",
    config = function()
      require("which-key").setup({
        preset = "helix",
        spec = {
          { mode = { "n", "v" }, { "<leader>l", group = "lsp" } },
          { "<leader>f", group = "find" },
          { "<leader>g", group = "git" },
          { "<leader>gh", group = "hunks" },
          { "<leader>n", group = "annotations" },
          { "<leader>q", group = "session" },
        },
      })
    end,
  },

  {
    url = "brenoprata10/nvim-highlight-colors",
    config = function()
      require("nvim-highlight-colors").setup({
        render = "background",
        enable_tailwind = true,
      })
    end,
  },

  {
    url = "folke/todo-comments.nvim",
    config = function()
      require("todo-comments").setup({})
    end,
  },
}
