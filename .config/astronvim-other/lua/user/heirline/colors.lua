return function(opts)
	-- local C = require("astronvim.utils.status.env").fallback_colors
	-- local get_hlgroup = require("astronvim.utils").get_hlgroup
	-- local Normal = get_hlgroup("Normal", { fg = C.fg, bg = C.bg })
	opts.buffer_active_path_fg = "#b4bbbe"
	opts.buffer_active_bg = "#2c323c"

	return opts
end
