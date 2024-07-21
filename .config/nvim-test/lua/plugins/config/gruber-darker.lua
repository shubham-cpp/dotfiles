---@diagnostic disable-next-line: missing-fields
require('gruber-darker').setup({})
vim.cmd.colorscheme 'gruber-darker'
local c = require 'gruber-darker.palette'
-- local vim_hl = require("gruber-darker.highlights.vim").highlights
local gruber_hl = require('gruber-darker.highlights.colorscheme').highlights
vim.api.nvim_set_hl(0, 'MatchParenCur', { bold = true })
vim.api.nvim_set_hl(0, 'lualine_c_12', { bg = c['bg+4']:to_string() })
vim.api.nvim_set_hl(0, 'lualine_c_buffers_inactive', { bg = c['bg+1']:to_string() })
vim.api.nvim_set_hl(0, 'lualine_x_tabs_active', { bg = c['bg+4']:to_string() })
vim.api.nvim_set_hl(0, 'lualine_x_tabs_inactive', { bg = c['bg+1']:to_string() })
vim.api.nvim_set_hl(0, '@lsp.type.keyword.lua', { fg = c['brown']:to_string(), bold = false })
vim.api.nvim_set_hl(0, '@lsp.type.event.lua', { link = 'GruberDarkerYellowBold' })
vim.api.nvim_set_hl(
  0,
  'QuickScopePrimary',
  { fg = c['yellow']:to_string(), bg = c['bg+1']:to_string(), underline = true, bold = true }
)
vim.api.nvim_set_hl(
  0,
  'QuickScopeSecondary',
  { fg = c['brown']:to_string(), bg = c['bg+1']:to_string(), underline = true, bold = true }
)

-- vim.api.nvim_set_hl(
--   0,
--   'QuickScopeSecondary',
--   { fg = c['brown']:to_string(), bg = c['bg+1']:to_string(), underline = true, bold = true }
-- )

--[[
local Color = require("gruber-darker.color")
local M = {}

---@type table<string, Color>
M = {
	none = Color.none(),
	fg = Color.new(0xe4e4e4),
	["fg+1"] = Color.new(0xf4f4ff),
	["fg+2"] = Color.new(0xf5f5f5),
	white = Color.new(0xffffff),
	black = Color.new(0x000000),
	["bg-1"] = Color.new(0x101010),
	bg = Color.new(0x181818),
	["bg+1"] = Color.new(0x282828),
	["bg+2"] = Color.new(0x453d41),
	["bg+3"] = Color.new(0x484848),
	["bg+4"] = Color.new(0x52494e),
	["red-1"] = Color.new(0xc73c3f),
	red = Color.new(0xf43841),
	["red+1"] = Color.new(0xff4f58),
	green = Color.new(0x73d936),
	yellow = Color.new(0xffdd33),
	brown = Color.new(0xcc8c3c),
	quartz = Color.new(0x95a99f),
	["niagara-2"] = Color.new(0x303540),
	["niagara-1"] = Color.new(0x565f73),
	niagara = Color.new(0x96a6c8),
	wisteria = Color.new(0x9e95c7),
}

return M
--]]
