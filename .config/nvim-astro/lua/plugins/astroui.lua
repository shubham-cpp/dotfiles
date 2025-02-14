---@type LazySpec
return {
  {
    "AstroNvim/astroui",
    ---@type AstroUIOpts
    opts = {
      -- change colorscheme
      colorscheme = "astrodark",
      -- colorscheme = "jellybeans",
      -- AstroUI allows you to easily modify highlight groups easily for any and all colorschemes
      highlights = {
        --   init = { -- this table overrides highlights in all themes
        --     -- Normal = { bg = "#000000" },
        --   },
        --   astrodark = { -- a table of overrides/changes when applying the astrotheme theme
        --     -- Normal = { bg = "#000000" },
        --   },
        astrodark = {
          QuickScopePrimary = { fg = "#dfbb78", bg = "#505050", bold = true, undercurl = true },
          QuickScopeSecondary = { fg = "#61afef", bg = "#505050", bold = true, undercurl = true },
        },
      },
      -- modify variables used by heirline but not defined in the setup call directly
      status = {
        colors = function(hl)
          hl.buffer_active_bg = "#242642"
          return hl
        end,
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
  -- {
  --   "wtfox/jellybeans.nvim",
  --   enabled = true,
  --   opts = {
  --     ---@type 'dark'|'light'
  --     style = "dark",
  --     transparent = true,
  --   },
  -- },
}
