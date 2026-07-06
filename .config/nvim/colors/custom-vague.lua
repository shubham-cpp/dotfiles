local colors = {
  bg = "#141415",
  inactive_bg = "#1c1c24",
  fg = "#cdcdcd",
  float_border = "#878787",
  line = "#252530",
  comment = "#606079",
  builtin = "#b4d4cf",
  func = "#c48282",
  string = "#e8b589",
  number = "#e0a363",
  property = "#c3c3d5",
  constant = "#aeaed1",
  parameter = "#bb9dbd",
  visual = "#333738",
  error = "#d8647e",
  warning = "#f3be7c",
  hint = "#7e98e8",
  operator = "#90a0b5",
  keyword = "#6e94b2",
  type = "#9bb4bc",
  search = "#405065",
  plus = "#7fa563",
  delta = "#f3be7c",
  diff_add = "#293125",
  diff_change = "#41362a",
  diff_delete = "#3b242a",
  diff_text = "#6d583e",
}

vim.g.custom_vague_palette = colors

require("mini.base16").setup({
  palette = {
    base00 = colors.bg,
    base01 = colors.inactive_bg,
    base02 = colors.line,
    base03 = colors.comment,
    base04 = colors.float_border,
    base05 = colors.fg,
    base06 = colors.property,
    base07 = "#f4f4f4",
    base08 = colors.error,
    base09 = colors.number,
    base0A = colors.warning,
    base0B = colors.plus,
    base0C = colors.builtin,
    base0D = colors.func,
    base0E = colors.keyword,
    base0F = colors.parameter,
  },
  use_cterm = true,
  plugins = {
    default = true,
    ["nvim-mini/mini.nvim"] = true,
    ["saghen/blink.cmp"] = true,
  },
})

vim.g.colors_name = "custom-vague"

local hi = function(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

local link = function(group, target)
  hi(group, { link = target })
end

hi("Normal", { fg = colors.fg, bg = colors.bg })
hi("NormalNC", { fg = colors.fg, bg = colors.bg })
hi("NormalFloat", { fg = colors.fg, bg = colors.inactive_bg })
hi("FloatBorder", { fg = colors.float_border, bg = colors.inactive_bg })
hi("FloatTitle", { link = "NormalFloat" })
hi("WinSeparator", { fg = colors.float_border })
hi("LineNr", { fg = colors.comment })
hi("CursorLine", { bg = colors.line })
hi("CursorLineNr", { fg = colors.fg })
hi("ColorColumn", { bg = colors.line })
hi("Folded", { fg = colors.comment, bg = colors.line })
hi("SignColumn", { fg = colors.fg, bg = colors.bg })
hi("Visual", { bg = colors.visual })
hi("Search", { fg = colors.fg, bg = colors.search })
hi("IncSearch", { fg = colors.bg, bg = colors.search })
hi("CurSearch", { fg = colors.fg, bg = colors.search })
hi("MatchParen", { fg = colors.fg, bg = colors.visual })
hi("Pmenu", { fg = colors.fg, bg = colors.line })
hi("PmenuSel", { fg = colors.fg, bg = colors.visual })
hi("PmenuSbar", { bg = colors.line })
hi("PmenuThumb", { bg = colors.comment })
hi("TabLine", { fg = colors.comment, bg = colors.inactive_bg })
hi("TabLineSel", { fg = colors.fg, bg = colors.bg, bold = true })
hi("TabLineFill", { bg = colors.bg })

local barbar_statuses = {
  Current = { fg = colors.fg, bg = colors.bg, accent = colors.builtin, bold = true },
  Visible = { fg = colors.fg, bg = colors.inactive_bg, accent = colors.type, bold = false },
  Inactive = { fg = colors.comment, bg = colors.inactive_bg, accent = colors.comment },
  Alternate = { fg = colors.fg, bg = colors.line, accent = colors.parameter, bold = true },
}

for status, spec in pairs(barbar_statuses) do
  local group = "Buffer" .. status
  hi(group, { fg = spec.fg, bg = spec.bg, bold = spec.bold })
  hi(group .. "Icon", { fg = spec.accent, bg = spec.bg, bold = spec.bold })
  hi(group .. "Index", { fg = spec.accent, bg = spec.bg, bold = spec.bold })
  hi(group .. "Number", { link = group .. "Index" })
  hi(group .. "Btn", { fg = spec.fg, bg = spec.bg })
  hi(group .. "Sign", { fg = spec.accent, bg = spec.bg, bold = spec.bold })
  hi(group .. "SignRight", { link = group .. "Sign" })
  hi(group .. "Mod", { fg = colors.warning, bg = spec.bg, bold = true })
  hi(group .. "ModBtn", { link = group .. "Mod" })
  hi(group .. "Pin", { fg = spec.accent, bg = spec.bg, bold = true })
  hi(group .. "PinBtn", { link = group .. "Pin" })
  hi(group .. "Target", { fg = colors.error, bg = spec.bg, bold = true })
  hi(group .. "ADDED", { fg = colors.plus, bg = spec.bg })
  hi(group .. "CHANGED", { fg = colors.delta, bg = spec.bg })
  hi(group .. "DELETED", { fg = colors.error, bg = spec.bg })
  hi(group .. "ERROR", { fg = colors.error, bg = spec.bg, bold = true })
  hi(group .. "WARN", { fg = colors.warning, bg = spec.bg, bold = true })
  hi(group .. "INFO", { fg = colors.constant, bg = spec.bg })
  hi(group .. "HINT", { fg = colors.hint, bg = spec.bg })
end

hi("BufferOffset", { fg = colors.comment, bg = colors.bg })
hi("BufferScrollArrow", { fg = colors.float_border, bg = colors.bg })
hi("BufferTabpageFill", { bg = colors.bg })
hi("BufferTabpages", { fg = colors.comment, bg = colors.bg, bold = true })
hi("BufferTabpagesSep", { fg = colors.float_border, bg = colors.bg })

hi("Comment", { fg = colors.comment, italic = true })
hi("String", { fg = colors.string, italic = true })
hi("Character", { fg = colors.string })
hi("Number", { fg = colors.number })
hi("Boolean", { fg = colors.number, bold = true })
hi("Float", { fg = colors.number })
hi("Function", { fg = colors.func })
hi("Identifier", { fg = colors.constant })
hi("Include", { fg = colors.keyword })
hi("Keyword", { fg = colors.keyword })
hi("Statement", { fg = colors.keyword })
hi("Conditional", { fg = colors.keyword })
hi("Exception", { fg = colors.keyword })
hi("Label", { fg = colors.keyword })
hi("Repeat", { fg = colors.keyword })
hi("Operator", { fg = colors.operator })
hi("Constant", { fg = colors.constant })
hi("Macro", { fg = colors.constant })
hi("PreProc", { fg = colors.constant })
hi("Define", { fg = colors.comment })
hi("PreCondit", { fg = colors.comment })
hi("StorageClass", { fg = colors.constant })
hi("Special", { fg = colors.builtin })
hi("SpecialChar", { fg = colors.keyword })
hi("SpecialComment", { fg = colors.keyword })
hi("Tag", { fg = colors.builtin })
hi("Type", { fg = colors.type })
hi("Structure", { fg = colors.constant })
hi("Title", { fg = colors.property })
hi("Typedef", { fg = colors.constant })
hi("Delimiter", { fg = colors.fg })
hi("Todo", { fg = colors.func, italic = true })

hi("DiagnosticError", { fg = colors.error, bold = true })
hi("DiagnosticWarn", { fg = colors.warning, bold = true })
hi("DiagnosticInfo", { fg = colors.constant, italic = true })
hi("DiagnosticHint", { fg = colors.hint })
hi("DiagnosticOk", { fg = colors.plus })
hi("DiagnosticUnderlineError", { sp = colors.error, undercurl = true })
hi("DiagnosticUnderlineWarn", { sp = colors.warning, undercurl = true, bold = true })
hi("DiagnosticUnderlineInfo", { sp = colors.constant, undercurl = true })
hi("DiagnosticUnderlineHint", { sp = colors.hint, undercurl = true })
hi("DiagnosticUnderlineOk", { sp = colors.plus, undercurl = true })
hi("DiagnosticVirtualTextError", { fg = colors.error, bold = true })
hi("DiagnosticVirtualTextWarn", { fg = colors.warning, bold = true })
hi("DiagnosticVirtualTextInfo", { fg = colors.constant, italic = true })
hi("DiagnosticVirtualTextHint", { fg = colors.hint })
hi("DiagnosticVirtualTextOk", { fg = colors.plus })

hi("Added", { fg = colors.plus })
hi("Changed", { fg = colors.delta })
hi("Removed", { fg = colors.error })
hi("DiffAdd", { bg = colors.diff_add })
hi("DiffChange", { bg = colors.diff_change })
hi("DiffDelete", { bg = colors.diff_delete })
hi("DiffText", { bg = colors.diff_text })

hi("LspCodeLens", { fg = colors.comment, italic = true })
hi("LspCodeLensSeparator", { fg = colors.comment })
hi("LspReferenceRead", { bg = colors.visual })
hi("LspReferenceText", { bg = colors.visual })
hi("LspReferenceWrite", { bg = colors.visual })
hi("LspInlayHint", { fg = colors.comment, bg = colors.inactive_bg })

hi("@variable", { fg = colors.fg })
hi("@variable.member", { fg = colors.builtin })
hi("@variable.parameter", { fg = colors.parameter })
hi("@constant.builtin", { fg = colors.number, bold = true })
hi("@constructor", { fg = colors.constant })
hi("@constructor.lua", { fg = colors.type })
hi("@function.builtin", { fg = colors.func })
hi("@function.call", { fg = colors.parameter })
hi("@function.method.call", { fg = colors.type })
hi("@keyword.return", { fg = colors.keyword, italic = true })
hi("@property", { fg = colors.property })
hi("@type.builtin", { fg = colors.builtin, bold = true })
hi("@type.declaration", { fg = colors.constant })
hi("@markup.heading", { fg = colors.keyword, bold = true })
hi("@markup.link.label", { fg = colors.string, underline = true })
hi("@markup.link.url", { fg = colors.func })
hi("@markup.list", { fg = colors.func })
hi("@markup.quote", { fg = colors.comment })
hi("@markup.raw", { fg = colors.constant })
hi("@tag.delimiter", { fg = colors.fg })
link("@diff.plus", "DiffAdd")
link("@diff.delta", "DiffChange")
link("@diff.minus", "DiffDelete")
link("@function.macro", "Macro")
link("@keyword.import", "PreProc")
link("@punctuation.special", "SpecialChar")
link("@string.special.symbol", "Identifier")
link("@tag.attribute", "Special")
link("htmlTagName", "Special")
link("tsxTagName", "Conditional")
link("@type.definition", "Typedef")

link("@lsp.type.builtinConstant", "@constant.builtin")
link("@lsp.type.builtinType", "@type.builtin")
link("@lsp.type.class", "Structure")
link("@lsp.type.comment", "Comment")
link("@lsp.type.enum", "Structure")
link("@lsp.type.enumMember", "@variable.member")
link("@lsp.type.function", "@function.call")
link("@lsp.type.generic", "@type")
link("@lsp.type.interface", "Structure")
link("@lsp.type.macro", "Macro")
link("@lsp.type.method", "@function.method.call")
link("@lsp.type.namespace", "@module")
link("@lsp.type.parameter", "@variable.parameter")
link("@lsp.type.property", "@property")
link("@lsp.type.selfParameter", "Special")
link("@lsp.type.typeParameter", "Typedef")
link("@lsp.type.variable", "@variable")
link("@lsp.typemod.function.builtin", "@function.builtin")
link("@lsp.typemod.function.defaultLibrary", "@function.builtin")
link("@lsp.typemod.function.definition", "@function")
link("@lsp.typemod.variable.defaultLibrary", "@constant.builtin")
link("@lsp.typemod.variable.definition", "@property")

hi("BlinkCmpMenu", { fg = colors.fg, bg = colors.line })
hi("BlinkCmpMenuBorder", { fg = colors.float_border, bg = colors.line })
hi("BlinkCmpDoc", { fg = colors.fg, bg = colors.inactive_bg })
hi("BlinkCmpDocBorder", { fg = colors.float_border, bg = colors.inactive_bg })
hi("BlinkCmpSignatureHelp", { fg = colors.fg, bg = colors.inactive_bg })
hi("BlinkCmpSignatureHelpBorder", { fg = colors.float_border, bg = colors.inactive_bg })
hi("BlinkCmpLabelMatch", { fg = colors.delta, bold = true })
hi("BlinkCmpLabelDeprecated", { fg = colors.error, strikethrough = true })
hi("BlinkCmpKind", { fg = colors.comment })
hi("BlinkCmpSource", { fg = colors.comment })

hi("NavicIconsFile", { fg = colors.hint })
hi("NavicIconsModule", { fg = colors.hint })
hi("NavicIconsNamespace", { fg = colors.hint })
hi("NavicIconsPackage", { fg = colors.hint })
hi("NavicIconsClass", { fg = colors.type })
hi("NavicIconsMethod", { fg = colors.func })
hi("NavicIconsProperty", { fg = colors.property })
hi("NavicIconsField", { fg = colors.property })
hi("NavicIconsConstructor", { fg = colors.func })
hi("NavicIconsEnum", { fg = colors.constant })
hi("NavicIconsInterface", { fg = colors.type })
hi("NavicIconsFunction", { fg = colors.func })
hi("NavicIconsVariable", { fg = colors.parameter })
hi("NavicIconsConstant", { fg = colors.constant })
hi("NavicIconsString", { fg = colors.string, italic = true })
hi("NavicIconsNumber", { fg = colors.number })
hi("NavicIconsBoolean", { fg = colors.number, bold = true })
hi("NavicIconsArray", { fg = colors.number })
hi("NavicIconsObject", { fg = colors.number })
hi("NavicIconsKey", { fg = colors.property })
hi("NavicIconsNull", { fg = colors.comment })
hi("NavicIconsEnumMember", { fg = colors.error })
hi("NavicIconsStruct", { fg = colors.type })
hi("NavicIconsEvent", { fg = colors.warning })
hi("NavicIconsOperator", { fg = colors.operator })
hi("NavicIconsTypeParameter", { fg = colors.parameter })
hi("NavicText", { fg = colors.fg })
hi("NavicSeparator", { fg = colors.comment })

hi("MiniPickBorder", { fg = colors.float_border, bg = colors.inactive_bg })
hi("MiniPickBorderBusy", { fg = colors.warning, bg = colors.inactive_bg })
hi("MiniPickBorderText", { fg = colors.parameter, bg = colors.inactive_bg })
hi("MiniPickPrompt", { fg = colors.constant, bg = colors.inactive_bg })
hi("MiniPickPromptCaret", { fg = colors.warning, bg = colors.inactive_bg })
hi("MiniPickIconDirectory", { fg = colors.hint })
hi("MiniPickIconFile", { fg = colors.fg })
hi("MiniPickMatchCurrent", { fg = colors.fg, bg = colors.visual })
hi("MiniPickMatchMarked", { fg = colors.fg, bg = colors.line, bold = true })
hi("MiniFilesBorder", { fg = colors.float_border, bg = colors.inactive_bg })
hi("MiniFilesCursorLine", { bg = colors.line })
hi("MiniFilesDirectory", { fg = colors.hint })
hi("MiniFilesFile", { fg = colors.fg })
hi("MiniDiffSignAdd", { fg = colors.plus })
hi("MiniDiffSignChange", { fg = colors.delta })
hi("MiniDiffSignDelete", { fg = colors.error })
hi("MiniDiffOverContext", { bg = colors.line })
hi("MiniHipatternsFixme", { fg = colors.bg, bg = colors.error, bold = true })
hi("MiniHipatternsHack", { fg = colors.bg, bg = colors.warning, bold = true })
hi("MiniHipatternsTodo", { fg = colors.bg, bg = colors.hint, bold = true })
hi("MiniHipatternsNote", { fg = colors.bg, bg = colors.constant, bold = true })
hi("MiniTablineCurrent", { fg = colors.fg, bg = colors.bg, bold = true })
hi("MiniTablineVisible", { fg = colors.fg, bg = colors.inactive_bg })
hi("MiniTablineHidden", { fg = colors.comment, bg = colors.inactive_bg })
hi("MiniTablineModifiedCurrent", { fg = colors.warning, bg = colors.bg, bold = true })
hi("MiniTablineModifiedVisible", { fg = colors.warning, bg = colors.inactive_bg })
hi("MiniTablineModifiedHidden", { fg = colors.warning, bg = colors.inactive_bg })
hi("MiniTablineTabpagesection", { fg = colors.fg, bg = colors.line, bold = true })
hi("MiniCursorwordCurrent", {})
-- hi("MiniCursorword", { bg = colors.property, underline = true })

hi("CmpItemAbbrDeprecated", { fg = colors.error, strikethrough = true })
hi("CmpItemAbbrMatch", { fg = colors.delta, bold = true })
hi("CmpItemAbbrMatchFuzzy", { fg = colors.delta, bold = true })
hi("CmpItemKind", { fg = colors.comment })
