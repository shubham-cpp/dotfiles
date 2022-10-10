local xresources = require 'beautiful.xresources'
local gears = require 'gears'

local gfs = gears.filesystem
local dpi = xresources.apply_dpi

-- local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
local icon_dir = gfs.get_xdg_data_home() .. 'icons/Papirus-Dark/symbolic/actions'
if not gfs.dir_readable(icon_dir) then
  require('naughty').notify({
    preset = require('naughty').config.presets.critical,
    title = 'Current Layout',
    timeout = 2,
    text = 'icon_dir ' .. icon_dir .. ' does not exist',
  })
end

local theme_assets = require 'beautiful.theme_assets'
local themes_path = gfs.get_themes_dir()
local theme = {}
-- theme.dir                                       = homedir .. "/.config/awesome/themes/powerarrow-dark"
-- theme.wallpaper                                 = theme.dir .. "/wall.png"
theme.font = 'FuraCode Nerd Font Medium 9'
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
theme.menu_height = dpi(15)
theme.menu_width = dpi(100)

-- theme.awesome_icon = theme_assets.awesome_icon(theme.menu_height, theme.bg_focus, theme.fg_focus)
-- theme.menu_submenu_icon = theme.dir .. "/icons/submenu.png"

-- local taglist_square_size = dpi(4)
-- theme.taglist_squares_sel = theme_assets.taglist_squares_sel(taglist_square_size, theme.fg_normal)
-- theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(taglist_square_size, theme.fg_normal)
theme.layout_fairh = themes_path .. 'default/layouts/fairhw.png'
theme.layout_fairv = themes_path .. 'default/layouts/fairvw.png'
theme.layout_floating = themes_path .. 'default/layouts/floatingw.png'
theme.layout_magnifier = themes_path .. 'default/layouts/magnifierw.png'
theme.layout_max = themes_path .. 'default/layouts/maxw.png'
theme.layout_fullscreen = themes_path .. 'default/layouts/fullscreenw.png'
theme.layout_tilebottom = themes_path .. 'default/layouts/tilebottomw.png'
theme.layout_tileleft = themes_path .. 'default/layouts/tileleftw.png'
theme.layout_tile = themes_path .. 'default/layouts/tilew.png'
theme.layout_tiletop = themes_path .. 'default/layouts/tiletopw.png'
theme.layout_spiral = themes_path .. 'default/layouts/spiralw.png'
theme.layout_dwindle = themes_path .. 'default/layouts/dwindlew.png'
theme.layout_cornernw = themes_path .. 'default/layouts/cornernww.png'
theme.layout_cornerne = themes_path .. 'default/layouts/cornernew.png'
theme.layout_cornersw = themes_path .. 'default/layouts/cornersww.png'
theme.layout_cornerse = themes_path .. 'default/layouts/cornersew.png'
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

theme.titlebar_close_button_focus = icon_dir .. '/window-close-symbolic.svg'
theme.titlebar_close_button_normal = icon_dir .. '/window-close-symbolic.svg'

theme.titlebar_ontop_button_focus_active = icon_dir .. '/window-keep-above.svg'
theme.titlebar_ontop_button_focus_inactive = icon_dir .. '/window-keep-above.svg'
theme.titlebar_ontop_button_normal_active = icon_dir .. '/window-keep-below.svg'
theme.titlebar_ontop_button_normal_inactive = icon_dir .. '/window-keep-below.svg'

theme.titlebar_sticky_button_focus_active = icon_dir .. '/view-pin-symbolic.svg'
theme.titlebar_sticky_button_focus_inactive = icon_dir .. '/view-pin-symbolic.svg'
theme.titlebar_sticky_button_normal_active = icon_dir .. '/pane-hide-symbolic.svg'
theme.titlebar_sticky_button_normal_inactive = icon_dir .. '/pane-hide-symbolic.svg'

-- theme.titlebar_floating_button_focus_active = theme.dir .. "/icons/titlebar/floating_focus_active.png"
-- theme.titlebar_floating_button_normal_active = theme.dir .. "/icons/titlebar/floating_normal_active.png"
-- theme.titlebar_floating_button_focus_inactive = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
-- theme.titlebar_floating_button_normal_inactive = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active = icon_dir .. '/window-minimize-symbolic.svg'
theme.titlebar_maximized_button_normal_active = icon_dir .. '/window-minimize-symbolic.svg'
theme.titlebar_maximized_button_focus_inactive = icon_dir .. '/window-maximize-symbolic.svg'
theme.titlebar_maximized_button_normal_inactive = icon_dir .. '/window-maximize-symbolic.svg'

-- theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
-- theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
-- theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
-- theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.notification_max_width = dpi(480)
theme.notification_max_height = dpi(160)
theme.notification_icon_size = dpi(48)
theme.notification_border_width = dpi(3)
theme.notification_border_color = '#89b4fa'

theme.icon_theme = nil

return theme
