return function()
	local get_hlgroup = require("astronvim.utils").get_hlgroup
	-- get highlights from highlight groups
	local normal = get_hlgroup("Normal")
	local fg, bg = normal.fg, normal.bg
	local bg_alt = get_hlgroup("Visual").bg
	local green = get_hlgroup("String").fg
	local red = get_hlgroup("Error").fg
	-- return a table of highlights for telescope based on colors gotten from highlight groups
	return {
		TelescopeBorder = { fg = bg_alt, bg = bg },
		TelescopeNormal = { bg = bg },
		TelescopePreviewBorder = { fg = bg, bg = bg },
		TelescopePreviewNormal = { bg = bg },
		TelescopePreviewTitle = { fg = bg, bg = green },
		TelescopePromptBorder = { fg = bg_alt, bg = bg_alt },
		TelescopePromptNormal = { fg = fg, bg = bg_alt },
		TelescopePromptPrefix = { fg = red, bg = bg_alt },
		TelescopePromptTitle = { fg = bg, bg = red },
		TelescopeResultsBorder = { fg = bg, bg = bg },
		TelescopeResultsNormal = { bg = bg },
		TelescopeResultsTitle = { fg = bg, bg = bg },
		LspReferenceRead = { fg = fg, bg = bg_alt, bold = true },
		LspReferenceText = { fg = fg, bg = bg_alt, bold = true },
		LspReferenceWrite = { fg = fg, bg = bg_alt, bold = true },
		QuickScopePrimary = { fg = "#afff5f", bg = bg_alt, underline = true },
		QuickScopeSecondary = { fg = "#5fffff", bg = bg_alt, underline = true },
		-- hi LspReferenceRead cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
	}
end
