local gears = require('gears')
local gfs = gears.filesystem
-- local awful = require("awful")
-- local wibox = require("wibox")
local xresources = require('beautiful.xresources')
local dpi = xresources.apply_dpi

-- local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
-- local icon_dir = gfs.get_xdg_data_home() .. 'icons/Luna-Dark/symbolic/actions'
local icon_dir = '/usr/share/icons/Qogir-dark/24/actions'
if not gfs.dir_readable(icon_dir) then
	require('naughty').notify({
		preset = require('naughty').config.presets.critical,
		title = 'Current Layout',
		timeout = 2,
		text = 'icon_dir ' .. icon_dir .. ' does not exist',
	})
end

local theme = {}
-- theme.dir                                       = homedir .. "/.config/awesome/themes/powerarrow-dark"
-- theme.wallpaper                                 = theme.dir .. "/wall.png"
theme.font = 'FuraCode Nerd Font Medium 11'
theme.fg_normal = '#DDDDFF'
theme.fg_focus = '#EA6F81'
theme.fg_urgent = '#CC9393'
theme.bg_normal = '#1A1A1A'
theme.bg_focus = '#313131'
theme.bg_urgent = '#1A1A1A'
theme.border_width = dpi(3)
theme.border_normal = '#3F3F3F'
theme.border_focus = '#bd93f9'
theme.border_marked = '#CC9393'
theme.tasklist_bg_focus = '#1A1A1A'
theme.titlebar_bg_focus = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal
theme.titlebar_fg_focus = theme.fg_focus
theme.menu_height = dpi(16)
-- theme.menu_width =  dpi(140)
-- theme.menu_submenu_icon = theme.dir .. "/icons/submenu.png"
-- theme.taglist_squares_sel = theme.dir .. "/icons/square_sel.png"
-- theme.taglist_squares_unsel = theme.dir .. "/icons/square_unsel.png"
-- theme.layout_tile = theme.dir .. "/icons/tile.png"
-- theme.layout_tileleft = theme.dir .. "/icons/tileleft.png"
-- theme.layout_tilebottom = theme.dir .. "/icons/tilebottom.png"
-- theme.layout_tiletop = theme.dir .. "/icons/tiletop.png"
-- theme.layout_fairv = theme.dir .. "/icons/fairv.png"
-- theme.layout_fairh = theme.dir .. "/icons/fairh.png"
-- theme.layout_spiral = theme.dir .. "/icons/spiral.png"
-- theme.layout_dwindle = theme.dir .. "/icons/dwindle.png"
-- theme.layout_max = theme.dir .. "/icons/max.png"
-- theme.layout_fullscreen = theme.dir .. "/icons/fullscreen.png"
-- theme.layout_magnifier = theme.dir .. "/icons/magnifier.png"
-- theme.layout_floating = theme.dir .. "/icons/floating.png"
-- theme.widget_ac = theme.dir .. "/icons/ac.png"
-- theme.widget_battery = theme.dir .. "/icons/battery.png"
-- theme.widget_battery_low = theme.dir .. "/icons/battery_low.png"
-- theme.widget_battery_empty = theme.dir .. "/icons/battery_empty.png"
-- theme.widget_mem = theme.dir .. "/icons/mem.png"
-- theme.widget_cpu = theme.dir .. "/icons/cpu.png"
-- theme.widget_temp = theme.dir .. "/icons/temp.png"
-- theme.widget_net = theme.dir .. "/icons/net.png"
-- theme.widget_hdd = theme.dir .. "/icons/hdd.png"
-- theme.widget_music = theme.dir .. "/icons/note.png"
-- theme.widget_music_on = theme.dir .. "/icons/note_on.png"
-- theme.widget_vol = theme.dir .. "/icons/vol.png"
-- theme.widget_vol_low = theme.dir .. "/icons/vol_low.png"
-- theme.widget_vol_no = theme.dir .. "/icons/vol_no.png"
-- theme.widget_vol_mute = theme.dir .. "/icons/vol_mute.png"
-- theme.widget_mail = theme.dir .. "/icons/mail.png"
-- theme.widget_mail_on = theme.dir .. "/icons/mail_on.png"
theme.tasklist_plain_task_name = true
theme.tasklist_disable_icon = true
theme.useless_gap = dpi(2)
theme.gap_single_client = false

theme.titlebar_close_button_focus = icon_dir .. '/window-close.svg'
theme.titlebar_close_button_normal = icon_dir .. '/window-close.svg'

theme.titlebar_ontop_button_focus_active = icon_dir .. '/window-keep-above.svg'
theme.titlebar_ontop_button_focus_inactive = icon_dir .. '/window-keep-above.svg'
theme.titlebar_ontop_button_normal_active = icon_dir .. '/window-keep-below.svg'
theme.titlebar_ontop_button_normal_inactive = icon_dir .. '/window-keep-below.svg'

theme.titlebar_sticky_button_focus_active = icon_dir .. '/window-pin.svg'
theme.titlebar_sticky_button_focus_inactive = icon_dir .. '/window-pin.svg'
theme.titlebar_sticky_button_normal_active = icon_dir .. '/window-unpin.svg'
theme.titlebar_sticky_button_normal_inactive = icon_dir .. '/window-unpin.svg'

-- theme.titlebar_floating_button_focus_active = theme.dir .. "/icons/titlebar/floating_focus_active.png"
-- theme.titlebar_floating_button_normal_active = theme.dir .. "/icons/titlebar/floating_normal_active.png"
-- theme.titlebar_floating_button_focus_inactive = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
-- theme.titlebar_floating_button_normal_inactive = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active = icon_dir .. '/window-minimize.svg'
theme.titlebar_maximized_button_normal_active = icon_dir .. '/window-minimize.svg'
theme.titlebar_maximized_button_focus_inactive = icon_dir .. '/window-maximize.svg'
theme.titlebar_maximized_button_normal_inactive = icon_dir .. '/window-maximize.svg'

-- theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
-- theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
-- theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
-- theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.notification_max_width = dpi(480)
theme.notification_max_height = dpi(160)
theme.notification_icon_size = dpi(48)
theme.notification_border_width = dpi(3)
theme.notification_border_color = '#89b4fa'

return theme
