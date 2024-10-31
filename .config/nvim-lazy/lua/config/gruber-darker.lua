---@diagnostic disable-next-line: missing-fields
require("gruber-darker").setup({})
local c = require("gruber-darker.palette")

vim.api.nvim_set_hl(0, "MatchParenCur", { bold = true })

vim.api.nvim_set_hl(0, "@lsp.type.keyword.lua", { fg = c["brown"]:to_string(), bold = false })
vim.api.nvim_set_hl(0, "@lsp.type.event.lua", { link = "GruberDarkerYellowBold" })
vim.api.nvim_set_hl(0, "WinBar", { link = "TreesitterContext" })
-- vim.api.nvim_set_hl(0, 'WinBarNC', { link = 'NormalNC' })
vim.api.nvim_set_hl(
  0,
  "QuickScopePrimary",
  { fg = c["yellow"]:to_string(), bg = c["bg+1"]:to_string(), underline = true, bold = true }
)
vim.api.nvim_set_hl(
  0,
  "QuickScopeSecondary",
  { fg = c["brown"]:to_string(), bg = c["bg+1"]:to_string(), underline = true, bold = true }
)
