-- vim.cmd.colorscheme "custom-vague"
local u = require "config.utils"

vim.pack.add({ u.gh "vague-theme/vague.nvim" })

require("vague").setup({
  on_highlights = function(hl,colors)
    hl.Pmenu.bg = "NONE"
    hl.PmenuSel = { bg = colors.visual }

    -- blink.cmp window groups
    hl.BlinkCmpMenu = { link = "Pmenu" }
    hl.BlinkCmpLabel = { link = "Pmenu" }
    hl.BlinkCmpMenuSelection = { link = "PmenuSel" }

    -- Required: blink.cmp uses this exact group for matched characters.
    hl.BlinkCmpLabelMatch = { link = "CmpItemAbbrMatch" }
  end,
})

vim.cmd.colorscheme "vague"
