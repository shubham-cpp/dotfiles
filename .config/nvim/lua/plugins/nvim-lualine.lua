---@type LazySpec
return {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  opts = function()
    return {
      options = {
        globalstatus = vim.o.laststatus == 3,
      },
      sections = {
        lualine_a = {},
        lualine_b = { "branch" },
        lualine_c = {
          {
            "diagnostics",
            symbols = { error = " ", warn = " ", hint = " ", info = " " },
          },
          { "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
          { "filename",  path = 1, },
        },
        lualine_x = {
          {
            "diff",
            symbols = { added = " ", modified = " ", removed = " " },
          },
        },
        lualine_y = {
          "lsp_status",
          { "selectioncount", icon = "󰒉" },
          { "searchcount", icon = "" },
          { "progress", separator = " ", padding = { left = 1, right = 0 } },
          { "location", padding = { left = 0, right = 1 } },
        },
        lualine_z = {},
      },
      tabline = {
        lualine_a = {
          "buffers",
        },
        lualine_y = {
          "tabs",
        },
      },
      extensions = { "neo-tree", "lazy", "fzf" },
    }
  end,
}
