---@type LazySpec
return {
  {
    "AstroNvim/astroui",
    ---@type AstroUIOpts
    opts = {
      -- change colorscheme
      -- colorscheme = "astrodark",
      colorscheme = "vague",
      highlights = {
        -- this table overrides highlights in all themes
        --   init = { },
        astrodark = {
          QuickScopePrimary = { fg = "#dfbb78", bg = "#505050", bold = true, undercurl = true },
          QuickScopeSecondary = { fg = "#61afef", bg = "#505050", bold = true, undercurl = true },
        },
        vague = {
          ["@tag.attribute"] = { fg = "#c3c3d5" },
          StatusLine = { bg = "#181818" },
          WinBar = { bg = "#141415" },
          SnacksPickerMatch = { fg = "#f3be7c" },
          QuickScopePrimary = { fg = "#f3be7c", bg = "#333738", bold = true, undercurl = true },
          QuickScopeSecondary = { fg = "#7e98e8", bg = "#333738", bold = true, undercurl = true },
        },
      },
      -- modify variables used by heirline but not defined in the setup call directly
      status = {
        colors = function(hl)
          -- astrodark
          -- hl.buffer_active_bg = "#242642"
          hl.buffer_active_bg = "#393960"
          return hl
        end,
      },
    },
  },
  {
    "vague2k/vague.nvim",
    opts = { transparent = false },
  },
}
