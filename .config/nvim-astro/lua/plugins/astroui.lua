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
              -- blink.cmp
              BlinkCmpLabelMatch = { fg = "#f3be7c", bold = true },
              BlinkCmpLabelDetail = { fg = "#606079" },
              BlinkCmpLabelDescription = { fg = "#606079" },
              BlinkCmpKind = { fg = "#6e94b2" },
              BlinkCmpKindText = { fg = "#cdcdcd" },
              BlinkCmpKindMethod = { fg = "#c48282" },
              BlinkCmpKindFunction = { fg = "#c48282" },
              BlinkCmpKindConstructor = { fg = "#e8b589" },
              BlinkCmpKindField = { fg = "#c3c3d5" },
              BlinkCmpKindVariable = { fg = "#bb9dbd" },
              BlinkCmpKindClass = { fg = "#9bb4bc" },
              BlinkCmpKindInterface = { fg = "#9bb4bc" },
              BlinkCmpKindModule = { fg = "#6e94b2" },
              BlinkCmpKindProperty = { fg = "#c3c3d5" },
              BlinkCmpKindUnit = { fg = "#aeaed1" },
              BlinkCmpKindValue = { fg = "#aeaed1" },
              BlinkCmpKindEnum = { fg = "#9bb4bc" },
              BlinkCmpKindEnumMember = { fg = "#aeaed1" },
              BlinkCmpKindKeyword = { fg = "#6e94b2" },
              BlinkCmpKindSnippet = { fg = "#7e98e8" },
              BlinkCmpKindColor = { fg = "#aeaed1" },
              BlinkCmpKindFile = { fg = "#e8b589" },
              BlinkCmpKindReference = { fg = "#90a0b5" },
              BlinkCmpKindFolder = { fg = "#e8b589" },
              BlinkCmpKindConstant = { fg = "#aeaed1" },
              BlinkCmpKindStruct = { fg = "#9bb4bc" },
              BlinkCmpKindEvent = { fg = "#6e94b2" },
              BlinkCmpKindOperator = { fg = "#90a0b5" },
              BlinkCmpKindTypeParameter = { fg = "#9bb4bc" },
              BlinkCmpSource = { fg = "#606079" },
              BlinkCmpGhostText = { fg = "#606079" },
              BlinkCmpDocBorder = { fg = "#878787" },
              BlinkCmpMenuBorder = { fg = "#878787" },
              BlinkCmpSignatureHelpBorder = { fg = "#878787" },
            },
          },
        },
      },
    },
  },
}
