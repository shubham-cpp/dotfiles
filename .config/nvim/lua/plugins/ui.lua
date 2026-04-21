return {
  {
    "nvim-lualine/lualine.nvim",
    config = function()
      local diag_icons = require("core.icons")
      local tab_bufs = require("core.tab_buffers")
      local Buffers = require("lualine.components.buffers")
      local orig_buffers = Buffers.buffers
      function Buffers:buffers()
        local bufnrs = tab_bufs.get_bufnrs()
        local bufset = {}
        for _, b in ipairs(bufnrs) do bufset[b] = true end
        local result = {}
        Buffers.bufpos2nr = {}
        for _, b in ipairs(bufnrs) do
          result[#result + 1] = self:new_buffer(b, #result + 1)
          Buffers.bufpos2nr[#result] = b
        end
        return result
      end
      tab_bufs.setup()
      require("lualine").setup({
        options = { theme = "auto", icons_enabled = true },
        sections = {
          lualine_a = { { "mode", fmt = function(str) return str:sub(1, 1) end } },
          lualine_b = { "branch", "diff", { "diagnostics", symbols = diag_icons } },
          lualine_c = { "filename", { "navic", color_correction = "static" } },
          lualine_x = {},
          lualine_y = { "lsp_status", "progress" },
          lualine_z = { "location" },
        },
        inactive_sections = {
          lualine_a = {}, lualine_b = { "branch" }, lualine_c = { "filename" },
          lualine_x = {}, lualine_y = {}, lualine_z = {},
        },
        tabline = {
          lualine_a = {}, lualine_b = {},
          lualine_c = {
            {
              "buffers", mode = 0, show_filename_only = true, show_modified_status = true,
              buffers_color = {
                active = { fg = "#141415", bg = "#b4d4cf", gui = "bold" },
                inactive = { fg = "#606079", bg = "#1c1c24" },
              },
              symbols = { modified = " ●", alternate_file = "" },
            },
          },
          lualine_x = {}, lualine_y = {},
          lualine_z = {
            {
              "tabs", mode = 0, show_modified_status = true,
              tabs_color = {
                active = { fg = "#141415", bg = "#90a0b5", gui = "bold" },
                inactive = { fg = "#606079", bg = "#1c1c24" },
              },
              symbols = { modified = " ●" },
            },
          },
        },
      })
    end,
  },
  {
    "folke/which-key.nvim",
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
    "brenoprata10/nvim-highlight-colors",
    opts = { render = "background", enable_tailwind = true },
  },
  { "folke/todo-comments.nvim", opts = {} },
}
