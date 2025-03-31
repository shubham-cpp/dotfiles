--- Return all the attached lsps separated by commas
---@return string
local function attached_lsps()
  return vim
    .iter(vim.lsp.get_clients({ bufnr = 0 }) or {})
    :map(function(v)
      if v and v.config and v.config.name then return v.config.name end
    end)
    :join ","
end
---@type LazySpec
return {
  "nvim-lualine/lualine.nvim",
  event = "BufWinEnter",
  version = "*",
  dependencies = { "echasnovski/mini.icons" },
  opts = {
    options = {
      -- theme = "iceberg_dark",
      section_separators = { left = "", right = "" },
      component_separators = { left = "", right = "" },
    },
    sections = {
      lualine_a = { { "mode", fmt = function(str) return str:sub(1, 1) end, padding = { left = 1, right = 0 } } },
      lualine_c = {
        { "filetype", colored = true, icon_only = true, padding = { left = 1, right = 0 } },
        { "filename", path = 1 },
      },
      lualine_x = {
        { attached_lsps, cond = function() return vim.lsp.buf_is_attached(0) end },
        "fileformat",
      },
      lualine_y = {
        {
          "searchcount",
          icon = { "" },
        },
        { "selectioncount", icon = { "󰉸" } },
        "progress",
      },
    },
    inactive_winbar = {
      lualine_c = {
        { "filetype", colored = true, icon_only = true, padding = { left = 1, right = 0 }, color = { bg = "NONE" } },
        { "filename", color = { bg = "NONE", gui = "bold" } },
      },
    },
    winbar = {
      lualine_c = {
        {
          function()
            local ok, navic = pcall(require, "nvim-navic")
            return ok and navic.get_location() or ""
          end,
          cond = function()
            local ok, navic = pcall(require, "nvim-navic")
            return ok and navic.is_available() or false
          end,
          color = { bg = "NONE" },
        },
      },
    },
    -- tabline = {
    --   lualine_a = { "buffers" },
    --   lualine_z = { "tabs" },
    -- },
    extensions = { "quickfix", "mason", "nvim-dap-ui", "toggleterm", "neo-tree", "lazy", "fzf" },
  },
}
