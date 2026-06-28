local function statusline()
  return _G.MiniStatusline or require "mini.statusline"
end

local function hl(name)
  return "%#" .. name .. "#"
end

local function escape(value)
  return tostring(value):gsub("%%", "%%%%")
end

local function segment(group, value)
  if value == nil or value == "" then
    return ""
  end
  return hl(group) .. " " .. escape(value) .. " "
end

local function raw_segment(group, value)
  if value == nil or value == "" then
    return ""
  end
  return hl(group) .. " " .. value .. " "
end

local function mode_group()
  local mode = vim.fn.mode()
  if mode == "n" then
    return "ConfigStatuslineModeNormal"
  elseif mode == "i" or mode == "ic" or mode == "ix" then
    return "ConfigStatuslineModeInsert"
  elseif mode == "v" or mode == "V" or mode == "\22" or mode == "s" or mode == "S" then
    return "ConfigStatuslineModeVisual"
  elseif mode == "R" or mode == "Rv" then
    return "ConfigStatuslineModeReplace"
  elseif mode == "c" then
    return "ConfigStatuslineModeCommand"
  end
  return "ConfigStatuslineModeOther"
end

local function filename()
  local name = vim.api.nvim_buf_get_name(0)
  local display = name == "" and "[No Name]" or vim.fn.fnamemodify(name, ":.")
  local modified = vim.bo.modified and " +" or ""
  local readonly = vim.bo.readonly and " RO" or ""

  return escape(display .. modified .. readonly)
end

local function file_icon()
  if _G.MiniIcons == nil then
    return "", nil
  end

  local name = vim.api.nvim_buf_get_name(0)
  if name ~= "" then
    local icon, icon_hl = MiniIcons.get("file", name)
    return icon or "", icon_hl
  end

  if vim.bo.filetype ~= "" then
    local icon, icon_hl = MiniIcons.get("filetype", vim.bo.filetype)
    return icon or "", icon_hl
  end

  return "", nil
end

local function filename_segment()
  local icon, icon_hl = file_icon()
  if icon == "" then
    return segment("ConfigStatuslineFile", filename())
  end

  return table.concat({
    hl(icon_hl or "ConfigStatuslineFile"),
    " ",
    escape(icon),
    " ",
    hl "ConfigStatuslineFile",
    filename(),
    " ",
  })
end

local function lsp_clients()
  if statusline().is_truncated(90) then
    return ""
  end

  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients == 0 then
    return ""
  end

  local names = {}
  for _, client in ipairs(clients) do
    table.insert(names, client.name)
  end
  table.sort(names)

  return "  " .. table.concat(names, ", ")
end

local function branch()
  local summary = vim.b.minigit_summary or {}
  local name = summary.head_name or vim.b.gitsigns_head
  if name == nil or name == "" then
    return ""
  end
  if name == "HEAD" and summary.head ~= nil then
    name = summary.head:sub(1, 7)
  end
  return " " .. name
end

local function diff()
  if statusline().is_truncated(75) then
    return ""
  end

  local summary = vim.b.minidiff_summary or {}
  local add = summary.add or 0
  local change = summary.change or 0
  local delete = summary.delete or 0

  return table.concat({
    segment("ConfigStatuslineDiffAdd", add > 0 and "+" .. add or ""),
    segment("ConfigStatuslineDiffChange", change > 0 and "~" .. change or ""),
    segment("ConfigStatuslineDiffDelete", delete > 0 and "-" .. delete or ""),
  })
end

local function diagnostics()
  if statusline().is_truncated(80) or not vim.diagnostic.is_enabled({ bufnr = 0 }) then
    return ""
  end

  local count = vim.diagnostic.count(0)
  local severity = vim.diagnostic.severity
  local signs = vim.diagnostic.config().signs or {}
  local text = type(signs) == "table" and signs.text or {}
  text = type(text) == "table" and text or {}

  return table.concat({
    segment(
      "ConfigStatuslineDiagError",
      count[severity.ERROR] ~= nil and (text[severity.ERROR] or "") .. " " .. count[severity.ERROR] or ""
    ),
    segment(
      "ConfigStatuslineDiagWarn",
      count[severity.WARN] ~= nil and (text[severity.WARN] or "") .. " " .. count[severity.WARN] or ""
    ),
    segment(
      "ConfigStatuslineDiagInfo",
      count[severity.INFO] ~= nil and (text[severity.INFO] or "") .. " " .. count[severity.INFO] or ""
    ),
    segment(
      "ConfigStatuslineDiagHint",
      count[severity.HINT] ~= nil and (text[severity.HINT] or "") .. " " .. count[severity.HINT] or ""
    ),
  })
end

local function selected_lines()
  local mode = vim.fn.mode()
  if not (mode == "v" or mode == "V" or mode == "\22" or mode == "s" or mode == "S") then
    return ""
  end

  local count = math.abs(vim.fn.line "." - vim.fn.line "v") + 1
  return count > 1 and count .. "L" or ""
end

local function set_highlights()
  local p = vim.g.custom_vague_palette
    or {
      bg = "#0f0f0f",
      inactive_bg = "#171717",
      line = "#202020",
      fg = "#d7d7d7",
      comment = "#7c7c7c",
      plus = "#7aa89f",
      delta = "#c9a86a",
      error = "#d47766",
      parameter = "#a99ac7",
      warning = "#c9a86a",
    }
  local surface = p.inactive_bg or p.surface or p.bg
  local surface_2 = p.line or p.surface_2 or surface
  local muted = p.comment or p.muted or p.fg
  local green = p.plus or p.green or p.fg
  local amber = p.delta or p.warning or p.amber or p.fg
  local red = p.error or p.red or p.fg
  local mauve = p.parameter or p.mauve or p.fg

  local set = function(name, opts)
    vim.api.nvim_set_hl(0, name, opts)
  end

  set("StatusLine", { bg = p.bg, fg = p.fg })
  set("StatusLineNC", { bg = p.bg, fg = muted })
  set("ConfigStatuslineBg", { bg = p.bg, fg = p.fg })
  set("ConfigStatuslineSurface", { bg = surface, fg = p.fg })
  set("ConfigStatuslineBranch", { bg = surface, fg = p.fg })
  set("ConfigStatuslineDiff", { bg = surface, fg = muted })
  set("ConfigStatuslineDiffAdd", { bg = surface, fg = green })
  set("ConfigStatuslineDiffChange", { bg = surface, fg = amber })
  set("ConfigStatuslineDiffDelete", { bg = surface, fg = red })
  set("ConfigStatuslineFile", { bg = p.bg, fg = p.fg, bold = true })
  set("ConfigStatuslineRight", { bg = surface_2, fg = p.fg })
  set("ConfigStatuslineMacro", { bg = surface_2, fg = red, bold = true })
  set("ConfigStatuslineSelection", { bg = surface_2, fg = amber, bold = true })
  set("ConfigStatuslineLsp", { bg = surface, fg = mauve })
  set("ConfigStatuslineInactive", { bg = p.bg, fg = muted })
  set("ConfigStatuslineDiagError", { bg = surface, fg = red, bold = true })
  set("ConfigStatuslineDiagWarn", { bg = surface, fg = amber, bold = true })
  set("ConfigStatuslineDiagInfo", { bg = surface, fg = muted })
  set("ConfigStatuslineDiagHint", { bg = surface, fg = green })
  set("ConfigStatuslineModeNormal", { bg = p.bg, fg = p.fg, bold = true })
  set("ConfigStatuslineModeInsert", { bg = p.bg, fg = green, bold = true })
  set("ConfigStatuslineModeVisual", { bg = p.bg, fg = amber, bold = true })
  set("ConfigStatuslineModeReplace", { bg = p.bg, fg = red, bold = true })
  set("ConfigStatuslineModeCommand", { bg = p.bg, fg = mauve, bold = true })
  set("ConfigStatuslineModeOther", { bg = p.bg, fg = muted, bold = true })
end

local function active()
  local sl = statusline()
  local search = sl.section_searchcount({
    trunc_width = 75,
    options = { recompute = false },
  })
  local location = "%l:%c"
  local right = search ~= "" and " " .. search or location
  local macro = #vim.fn.reg_recording() > 0 and "@" .. vim.fn.reg_recording() or ""

  return table.concat({
    hl "ConfigStatuslineBg",
    hl(mode_group()) .. "▌",
    hl "ConfigStatuslineBranch",
    segment("ConfigStatuslineBranch", branch()),
    diff(),
    diagnostics(),
    hl "ConfigStatuslineBg",
    "%<",
    "%=",
    filename_segment(),
    "%=",
    segment("ConfigStatuslineLsp", lsp_clients()),
    segment("ConfigStatuslineMacro", macro),
    raw_segment("ConfigStatuslineRight", right),
    segment("ConfigStatuslineSelection", selected_lines()),
    hl "ConfigStatuslineBg",
  })
end

local function inactive()
  return table.concat({
    hl "ConfigStatuslineInactive",
    " " .. filename(),
    "%=",
    " %l:%c ",
  })
end

require("mini.statusline").setup({
  content = {
    active = active,
    inactive = inactive,
  },
  use_icons = true,
})
set_highlights()

vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("ConfigStatusline", { clear = true }),
  desc = "Refresh custom statusline highlights",
  callback = set_highlights,
})
