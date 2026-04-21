---@type LazySpec
return {
  {
    "AstroNvim/astroui",
    ---@type AstroUIOpts
    opts = {
      -- change colorscheme
      -- colorscheme = "astrodark",
      -- AstroUI allows you to easily modify highlight groups easily for any and all colorschemes
      highlights = {
        astrodark = {
          QuickScopePrimary = { fg = "#dfbb78", bg = "#505050", bold = true, undercurl = true },
          QuickScopeSecondary = { fg = "#61afef", bg = "#505050", bold = true, undercurl = true },
        },
      },
    },
  },
  {
    "vague2k/vague.nvim",
    lazy = true,
    enabled = true,
    opts = { transparent = false },
    specs = {
      {
        "AstroNvim/astroui",
        ---@type AstroUIOpts
        opts = {
          ---@type "vague"|"astrodark"
          colorscheme = "vague",
          status = {
            colors = function(hl)
              -- astrodark
              -- hl.buffer_active_bg = "#032240"
              hl.buffer_active_bg = "#393960"
              return hl
            end,
          },
          highlights = {
            vague = {
              ["@tag.attribute"] = { fg = "#c3c3d5" },
              StatusLine = { bg = "#181818" },
              WinBar = { bg = "#141415" },
              SnacksPickerMatch = { fg = "#f3be7c" },
              QuickScopePrimary = { fg = "#f3be7c", bg = "#333738", bold = true, undercurl = true },
              QuickScopeSecondary = { fg = "#7e98e8", bg = "#333738", bold = true, undercurl = true },
            },
          },
        },
      },
    },
  },
}
