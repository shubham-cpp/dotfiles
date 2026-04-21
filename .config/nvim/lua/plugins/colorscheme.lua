return {
  "vague2k/vague.nvim",
  priority = 1000,
  config = function()
    require("vague").setup({ transparent = false })
    vim.cmd.colorscheme("vague")

    local c = require("vague").get_palette()
    vim.api.nvim_set_hl(0, "@tag.attribute", { fg = c.property })
    vim.api.nvim_set_hl(0, "WinBar", { bg = c.bg })
    vim.api.nvim_set_hl(0, "NavicSeparator", { fg = c.delta, bg = "NONE", bold = true })
    vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = c.delta, bg = c.visual, bold = true, undercurl = true })
    vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = c.hint, bg = c.visual, bold = true, undercurl = true })

    local function mix_colors(fg_hex, bg_hex, pct)
      local function hex_to_rgb(h)
        h = h:gsub("#", "")
        return tonumber(h:sub(1, 2), 16), tonumber(h:sub(3, 4), 16), tonumber(h:sub(5, 6), 16)
      end
      local fr, fg, fb = hex_to_rgb(fg_hex)
      local br, bg_, bb = hex_to_rgb(bg_hex)
      local r = math.floor(fr * pct / 100 + br * (100 - pct) / 100)
      local g = math.floor(fg * pct / 100 + bg_ * (100 - pct) / 100)
      local b = math.floor(fb * pct / 100 + bb * (100 - pct) / 100)
      return string.format("#%02x%02x%02x", r, g, b)
    end

    local function adjust_lightness(hex, amount)
      local function hex_to_rgb(h)
        h = h:gsub("#", "")
        return tonumber(h:sub(1, 2), 16), tonumber(h:sub(3, 4), 16), tonumber(h:sub(5, 6), 16)
      end
      local r, g, b = hex_to_rgb(hex)
      r = math.min(255, math.max(0, r + amount))
      g = math.min(255, math.max(0, g + amount))
      b = math.min(255, math.max(0, b + amount))
      return string.format("#%02x%02x%02x", r, g, b)
    end

    local menu_bg = adjust_lightness(c.bg, -4)
    local doc_bg = adjust_lightness(c.bg, -8)

    vim.api.nvim_set_hl(0, "BlinkCmpMenu", { bg = menu_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpMenuBorder", { fg = c.bg, bg = menu_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpMenuSelection", { bg = mix_colors(c.comment, c.bg, 20) })
    vim.api.nvim_set_hl(0, "BlinkCmpScrollBarThumb", { bg = c.fg })
    vim.api.nvim_set_hl(0, "BlinkCmpScrollBarGutter", { bg = menu_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpLabel", { fg = c.fg })
    vim.api.nvim_set_hl(0, "BlinkCmpLabelDeprecated", { fg = c.error, strikethrough = true })
    vim.api.nvim_set_hl(0, "BlinkCmpLabelMatch", { fg = c.func, bold = true })
    vim.api.nvim_set_hl(0, "BlinkCmpLabelDetail", { fg = c.comment })
    vim.api.nvim_set_hl(0, "BlinkCmpLabelDescription", { fg = c.comment })
    vim.api.nvim_set_hl(0, "BlinkCmpSource", { fg = c.comment })
    vim.api.nvim_set_hl(0, "BlinkCmpGhostText", { fg = c.comment })
    vim.api.nvim_set_hl(0, "BlinkCmpDoc", { bg = doc_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpDocBorder", { fg = doc_bg, bg = doc_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpDocSeparator", { fg = c.comment })
    vim.api.nvim_set_hl(0, "BlinkCmpDocCursorLine", { bg = menu_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpSignatureHelp", { bg = menu_bg })
    vim.api.nvim_set_hl(0, "BlinkCmpSignatureHelpBorder", { fg = c.floatBorder })
    vim.api.nvim_set_hl(0, "BlinkCmpSignatureHelpActiveParameter", { fg = c.func, bold = true })

    local blink_kinds = {
      Constant = c.constant,
      Function = c.func,
      Variable = c.type,
      Snippet = c.constant,
      Text = c.string,
      Structure = c.type,
      Type = c.type,
      Keyword = c.keyword,
      Method = c.func,
      Constructor = c.func,
      Folder = c.keyword,
      Module = c.type,
      Property = c.property,
      Enum = c.func,
      Unit = c.type,
      Class = c.type,
      File = c.keyword,
      Interface = c.string,
      Color = c.fg,
      Reference = c.fg,
      EnumMember = c.constant,
      Value = c.string,
      Event = c.keyword,
      Operator = c.operator,
      TypeParameter = c.property,
    }

    for kind, color in pairs(blink_kinds) do
      local pill_bg = mix_colors(color, menu_bg, 25)
      vim.api.nvim_set_hl(0, "BlinkCmpKind" .. kind, { fg = color, bg = pill_bg })
    end
  end,
}
