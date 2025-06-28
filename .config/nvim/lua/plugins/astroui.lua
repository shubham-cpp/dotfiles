---@type LazySpec
return {
  {
    "oxfist/night-owl.nvim",
    enabled = true,
    lazy = true,
    opts = {
      transparent_background = true,
    },
  },
  {
    "vague2k/vague.nvim",
    enabled = true,
    lazy = true,
    opts = { transparent = false },
  },
  {
    "AstroNvim/astroui",
    ---@type AstroUIOpts
    opts = {
      ---@type "night-owl"|"vague"|"astrodark"
      colorscheme = "night-owl",
      status = {
        colors = function(hl)
          -- astrodark
          hl.buffer_active_bg = "#032240"
          -- hl.buffer_active_bg = "#393960"
          return hl
        end,
      },
      highlights = {
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
      -- Icons can be configured throughout the interface
      -- icons = {
      --   -- configure the loading of the lsp in the status line
      --   LSPLoading1 = "⠋",
      --   LSPLoading2 = "⠙",
      --   LSPLoading3 = "⠹",
      --   LSPLoading4 = "⠸",
      --   LSPLoading5 = "⠼",
      --   LSPLoading6 = "⠴",
      --   LSPLoading7 = "⠦",
      --   LSPLoading8 = "⠧",
      --   LSPLoading9 = "⠇",
      --   LSPLoading10 = "⠏",
      -- },
    },
  },
}
