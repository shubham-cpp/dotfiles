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
      -- Icons can be configured throughout the interface
      icons = {
        -- configure the loading of the lsp in the status line
        LSPLoading1 = "⠋",
        LSPLoading2 = "⠙",
        LSPLoading3 = "⠹",
        LSPLoading4 = "⠸",
        LSPLoading5 = "⠼",
        LSPLoading6 = "⠴",
        LSPLoading7 = "⠦",
        LSPLoading8 = "⠧",
        LSPLoading9 = "⠇",
        LSPLoading10 = "⠏",
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

              -- CmpItemKindSnippet = { link = "@lsp.type.macro" },
              -- CmpItemKindKeyword = { link = "@lsp.type.class" },
              -- CmpItemKindText = { link = "@lsp.type.comment" },
              -- CmpItemKindMethod = { link = "@lsp.type.method" },
              -- CmpItemKindConstructor = { link = "@lsp.typemod.function" },
              -- CmpItemKindFunction = { link = "@lsp.type.function" },
              -- CmpItemKindFolder = { link = "@lsp.type.macro" },
              -- CmpItemKindModule = { link = "@lsp.type.macro" },
              -- CmpItemKindConstant = { link = "@lsp.type.builtinConstant" },
              -- CmpItemKindField = { link = "@lsp.type.property" },
              -- CmpItemKindProperty = { link = "@lsp.type.property" },
              -- CmpItemKindEnum = { link = "@lsp.type.enum" },
              -- CmpItemKindUnit = { link = "@lsp.type.typeParameter" },
              -- CmpItemKindClass = { link = "@lsp.type.class" },
              -- CmpItemKindVariable = { link = "@lsp.type.variable" },
              -- CmpItemKindFile = { link = "@lsp.type.generic" },
              -- CmpItemKindInterface = { link = "@lsp.type.interface" },
              -- CmpItemKindColor = { link = "@lsp.type.generic" },
              -- CmpItemKindReference = { link = "@lsp.typemod.variable.defaultLibrary" },
              -- CmpItemKindEnumMember = { link = "@lsp.type.enumMember" },
              -- CmpItemKindStruct = { link = "@lsp.type.class" },
              -- CmpItemKindValue = { link = "@lsp.typemod.variable.definition" },
              -- CmpItemKindEvent = { link = "@lsp.typemod.function.defaultLibrary" },
              -- CmpItemKindOperator = { link = "@lsp.typemod.function.defaultLibrary" },
              -- CmpItemKindTypeParameter = { link = "@lsp.type.builtinType" },
              -- CmpItemKindCopilot = { link = "@lsp.type.comment" },
            },
          },
        },
      },
    },
  },
}
