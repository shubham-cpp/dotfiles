---@diagnostic disable: missing-fields
require('tokyonight').setup(
  ---@type tokyonight.Config
  {
    style = 'night',
    transparent = false,
    --- You can override specific highlights to use other groups or a hex color
    --- function will be called with a Highlights and ColorScheme table
    ---@param hl tokyonight.Highlights
    ---@param c ColorScheme
    on_highlights = function(hl, c)
      local prompt = '#2d3149'
      hl.TelescopeNormal = { bg = c.bg_dark, fg = c.fg_dark }
      hl.TelescopeBorder = { bg = c.bg_dark, fg = c.bg_dark }
      hl.TelescopePromptNormal = { bg = prompt }
      hl.TelescopePromptBorder = { bg = prompt, fg = prompt }
      hl.TelescopePromptTitle = { bg = prompt, fg = prompt }
      hl.TelescopePreviewTitle = { bg = c.bg_dark, fg = c.bg_dark }
      hl.TelescopeResultsTitle = { bg = c.bg_dark, fg = c.bg_dark }

      hl.QuickScopePrimary = { fg = c.todo, bg = c.fg_gutter, bold = true, underline = true }
      hl.QuickScopeSecondary = { fg = c.rainbow[2], bg = c.fg_gutter, bold = true, underline = true }
    end,
  }
)

vim.cmd.colorscheme 'tokyonight'
