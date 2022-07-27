--[[
		If LuaRocks is installed, make sure that packages installed through it are
		found (e.g. lgi). If LuaRocks is not installed, do nothing.
--]]
pcall(require, 'luarocks.loader')

--[[ Standard awesome library --]]
local gears = require('gears')
local gfs = gears.filesystem
local awful = require('awful')
require('awful.autofocus')
--[[ Widget and layout library]]
local wibox = require('wibox')
-- Theme handling library
local beautiful = require('beautiful')
-- Notification library
local naughty = require('naughty')
local menubar = require('menubar')
local hotkeys_popup = require('awful.hotkeys_popup')
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require('awful.hotkeys_popup.keys')
-- Awesome widgets {{{
local brightness_widget = require('widgets.brightness')
local calendar_widget = require('widgets.calendar')
local volume = require('widgets.vol')
local battery_widget = require('widgets.battery')
--}}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({
		preset = naughty.config.presets.critical,
		title = 'Oops, there were errors during startup!',
		text = awesome.startup_errors,
	})
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal('debug::error', function(err)
		-- Make sure we don't go into an endless error loop
		if in_error then
			return
		end
		in_error = true

		naughty.notify({
			preset = naughty.config.presets.critical,
			title = 'Oops, an error happened!',
			text = tostring(err),
		})
		in_error = false
	end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
if not beautiful.init(gfs.get_configuration_dir() .. 'utils/theme.lua') then
	local dpi = beautiful.xresources.apply_dpi
	beautiful.init({
		font = 'Monospace Bold 10',
		useless_gap = 3,
		gap_single_client = true,
		border_width = dpi(2),
		border_normal = '#3F3F3F',
		border_focus = '#fae3b0',
	})
end
-- beautiful.useless_gap = 2
-- beautiful.gap_single_client = false

-- This is used later as the default terminal and editor to run.
local terminal = os.getenv('TERMINAL') or 'xterm'
local editor = os.getenv('EDITOR') or 'nano'
local editor_cmd = terminal .. ' -e ' .. editor
local browser = os.getenv('BROWSER') or 'firefox'

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
local modkey = 'Mod4'
local altkey = 'Mod1'

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	awful.layout.suit.tile,
	-- awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	awful.layout.suit.max,
	awful.layout.suit.floating,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
	{
		'hotkeys',
		function()
			hotkeys_popup.show_help(nil, awful.screen.focused())
		end,
	},
	{ 'manual', terminal .. ' -e man awesome' },
	{ 'edit config', editor_cmd .. ' ' .. awesome.conffile },
	{ 'restart', awesome.restart },
	{
		'quit',
		function()
			awesome.quit()
		end,
	},
}

mymainmenu = awful.menu({
	items = {
		{ 'awesome', myawesomemenu, beautiful.awesome_icon },
		{ 'open terminal', terminal },
	},
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon, menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar
-- Create a textclock widget
local spr = wibox.widget.textbox(' | ')
-- local spr = wibox.widget({
-- 	shape = gears.shape.powerline,
-- 	widget = wibox.widget.separator,
-- })
local mytextclock = wibox.widget.textclock(' %a %b %d, %I:%M %p')
local systray = wibox.widget.systray()
systray:set_base_size(26)
-- local systray_margin = wibox.layout.margin()
-- systray_margin:set_margins(2)
-- systray_margin:set_widget(systray)

-- systray.forced_width = 38
local cw = calendar_widget({
	theme = 'outrun',
	placement = 'top_right',
	previous_month_button = 1,
	next_month_button = 3,
})
mytextclock:connect_signal('button::press', function(_, _, _, button)
	if button == 1 then
		cw.toggle()
	end
end)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
	awful.button({}, 1, function(t)
		t:view_only()
	end),
	awful.button({ modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({}, 3, awful.tag.viewtoggle),
	awful.button({ modkey }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
		end
	end),
	awful.button({}, 4, function(t)
		awful.tag.viewnext(t.screen)
	end),
	awful.button({}, 5, function(t)
		awful.tag.viewprev(t.screen)
	end)
)

local tasklist_buttons = gears.table.join(
	awful.button({}, 1, function(c)
		if c == client.focus then
			c.minimized = true
		else
			c:emit_signal('request::activate', 'tasklist', { raise = true })
		end
	end),
	awful.button({}, 3, function()
		awful.menu.client_list({ theme = { width = 250 } })
	end),
	awful.button({}, 4, function()
		awful.client.focus.byidx(1)
	end),
	awful.button({}, 5, function()
		awful.client.focus.byidx(-1)
	end)
)

awful.screen.connect_for_each_screen(function(s)
	-- Each screen has its own tag table.
	awful.tag({ '1', '2', '3', '4', '5', '6', '7', '8', '9' }, s, awful.layout.layouts[1])

	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()
	-- Create an imagebox widget which will contain an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(gears.table.join(
		awful.button({}, 1, function()
			awful.layout.inc(1)
		end),
		awful.button({}, 3, function()
			awful.layout.inc(-1)
		end),
		awful.button({}, 4, function()
			awful.layout.inc(1)
		end),
		awful.button({}, 5, function()
			awful.layout.inc(-1)
		end)
	))
	-- Create a taglist widget
	s.mytaglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.noempty,
		buttons = taglist_buttons,
	})

	-- Create a tasklist widget
	s.mytasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
	})

	-- Create the wibox
	s.mywibox = awful.wibar({ position = 'top', screen = s })

	-- Add widgets to the wibox
	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		{ -- Left widgets
			layout = wibox.layout.fixed.horizontal,
			mylauncher,
			s.mytaglist,
			s.mypromptbox,
		},
		s.mytasklist, -- Middle widget
		{ -- Right widgets
			layout = wibox.layout.fixed.horizontal,
			-- spr,
			-- awful.widget.watch('bash -c "dwm_bat"', 10),
			battery_widget({
				display_notification = true,
				show_current_level = true,
				font = beautiful.font,
				warning_msg_position = 'top_right',
				-- path_to_icons = '/usr/share/icons/Qogir/symbolic/status/',
				-- path_to_icons = gfs.get_xdg_data_home() .. 'icons/Fluent-grey-dark/symbolic/status/',
			}),
			spr,
			wibox.widget.textbox(' '),
			awful.widget.watch('bash -c "printf %d%% $(expr 100 - $(vmstat 1 2 | tail -1 | awk \'{print $15}\'))"', 3),
			spr,
			wibox.widget.textbox(' '),
			awful.widget.watch('sh -c "free -h | awk \'/^Mem/ {print $3}\'"', 5),
			spr,
			volume(),
			spr,
			brightness_widget({
				type = 'icon_and_text',
				program = 'xbacklight',
				font = beautiful.font,
				tooltip = true,
				timeout = 120,
			}),
			spr,
			awful.widget.watch('bash -c "curl wttr.in/411043\\?format=2"', 3600),
			spr,
			mytextclock,
			spr,
			s.mylayoutbox,
			spr,
			systray,
		},
	})
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
	awful.button({}, 3, function()
		mymainmenu:toggle()
	end),
	awful.button({}, 4, awful.tag.viewnext),
	awful.button({}, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings

-- Helper functions {{{

-- Defines to move the currently focused window.
-- to next/previous adjacent tag.
-- -1 for previous and 1 for next adjacent tag.
-- @param val number this is either be 1 or -1
-- @param total_tags number total number of tags to cycle through (default total no. tags created)
-- @see client.focus:move_to_tag()
local move_to_tag = function(val, total_tags)
	local step = val == 1 and 0 or 2
	local total_tags = total_tags or #awful.screen.focused().tags
	-- get current tag
	local t = client.focus and client.focus.first_tag or nil
	if t == nil then
		return
	end
	-- get previous tag (modulo 9 excluding 0 to wrap from 1 to 9)
	local tag = client.focus.screen.tags[(t.name - step) % total_tags + 1]
	awful.client.movetotag(tag)
	tag:view_only()
end
-- Skim through non empty tags.
-- @param direction number which can either be 1(forward)[default] or -1(backward)
-- @param s awful.screen Which monitor to view non empty tag. By default currently focused screen
-- @see lain.util.tag_view_nonempty()
local tag_view_nonempty = function(direction, s)
	direction = direction or 1
	s = s or awful.screen.focused()
	local tags = s.tags
	local sel = s.selected_tag

	local i = sel.index
	repeat
		i = i + direction

		-- Wrap around when we reach one of the bounds
		if i > #tags then
			i = i - #tags
		end
		if i < 1 then
			i = i + #tags
		end

		local t = tags[i]

		-- Stop when we get back to where we started
		if t == sel then
			break
		end

		-- If it's The One, view it.
		if #t:clients() > 0 then
			t:view_only()
			return
		end
	until false
end
-- Resize windows universally.
-- @param direction string which direction to go left,right,up,down
-- @param pixel number value to resize windows by, default is 0.05
-- @see awful.tag.incmwfact()
-- @see awful.client.incwfact()
local resize_window_tiled = function(direction, pixel)
	pixel = pixel or 0.05
	-- change pixel to negative if direction is "down/left"
	if direction == 'down' or direction == 'left' then
		pixel = -pixel
	end

	if direction == 'left' or direction == 'right' then
		awful.tag.incmwfact(pixel)
	else
		local master_win = awful.client.getmaster().window
		local current_win = client.focus.window
		if master_win == current_win then
			awful.tag.incmwfact(pixel)
		else
			pixel = direction == 'up' and -0.05 or 0.05
			awful.client.incwfact(pixel)
		end
	end
end

local resize_float = function(c, direction, move)
	local move = move or 20
	local direction = direction or 'up'
	local c = c or client.focus
	if direction == 'left' then
		c:relative_move(0, 0, -move, 0)
	elseif direction == 'right' then
		c:relative_move(0, 0, move, 0)
	elseif direction == 'down' then
		c:relative_move(0, 0, 0, move)
	else
		c:relative_move(0, 0, 0, -move)
	end
end

local resize_window = function(direction)
	local c = client.focus
	if c.floating then
		resize_float(c, direction)
	else
		resize_window_tiled(direction)
	end
end
-- }}}

globalkeys = gears.table.join(

	-- Tag browsing {{{

	-- This is for testing something or anything
	-- awful.key({ modkey,           }, "backslash",
	--         function ()
	-- local c = client.focus
	-- local layout_name = awful.layout.get().name
	-- naughty.notify({ preset = naughty.config.presets.normal,
	--                 title = "Current Layout",
	--                 timeout = 2,
	--                 text = tostring(c.floating) })
	-- end, {description = "Testing purpose", group = "tag"}),

	awful.key({ modkey }, '[', awful.tag.viewprev, { description = 'view previous', group = 'tag' }),
	awful.key({ modkey }, ']', awful.tag.viewnext, { description = 'view next', group = 'tag' }),
	awful.key({ modkey, 'Shift' }, '[', function()
		move_to_tag(-1)
	end, { description = 'move the focused client to previous tag', group = 'tag' }),
	awful.key({ modkey, 'Shift' }, ']', function()
		move_to_tag(1)
	end, { description = 'move the focused client to next tag', group = 'tag' }),
	awful.key({ modkey }, 'Tab', function()
		awful.tag.history.restore()
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'go back', group = 'tag' }),

	--- }}}

	-- Non-empty tag browsing {{{
	awful.key({ modkey }, 'grave', function()
		tag_view_nonempty(1)
	end, { description = 'view next nonempty', group = 'tag' }),

	awful.key({ modkey, 'Shift' }, 'grave', function()
		tag_view_nonempty(-1)
	end, { description = 'view  previous nonempty', group = 'tag' }),

	awful.key({ modkey }, 'comma', function()
		tag_view_nonempty(-1)
	end, { description = 'view  previous nonempty', group = 'tag' }),

	awful.key({ modkey }, 'period', function()
		tag_view_nonempty(1)
	end, { description = 'view next nonempty', group = 'tag' }),
	--- }}}

	-- By direction client focus    {{{

	awful.key({ modkey }, 'Down', function()
		awful.client.focus.global_bydirection('down')
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus down', group = 'client' }),
	awful.key({ modkey }, 'Up', function()
		awful.client.focus.global_bydirection('up')
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus up', group = 'client' }),
	awful.key({ modkey }, 'Left', function()
		awful.client.focus.global_bydirection('left')
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus left', group = 'client' }),
	awful.key({ modkey }, 'Right', function()
		awful.client.focus.global_bydirection('right')
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus right', group = 'client' }),
	awful.key({ modkey }, 'j', function()
		awful.client.focus.byidx(1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus down', group = 'client' }),
	awful.key({ modkey }, 'k', function()
		awful.client.focus.byidx(-1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus up', group = 'client' }),
	awful.key({ modkey }, 'h', function()
		awful.client.focus.global_bydirection('left')
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus left', group = 'client' }),
	awful.key({ modkey }, 'l', function()
		awful.client.focus.global_bydirection('right')
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'focus right', group = 'client' }),
	awful.key({ altkey }, 'Tab', function()
		awful.client.focus.byidx(1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'Focus to next Client', group = 'client' }),
	awful.key({ altkey, 'Shift' }, 'Tab', function()
		awful.client.focus.byidx(-1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = 'Focus to previous Client', group = 'client' }),
	-- }}}

	-- Layout manipulation {{{

	awful.key({ modkey, 'Shift' }, 'j', function()
		awful.client.swap.byidx(1)
	end, { description = 'swap with next client by index', group = 'client' }),
	awful.key({ modkey, 'Shift' }, 'k', function()
		awful.client.swap.byidx(-1)
	end, { description = 'swap with previous client by index', group = 'client' }),
	awful.key({ modkey, 'Shift' }, 'Left', function()
		awful.client.swap.byidx(-1)
	end, { description = 'swap with next client by index', group = 'client' }),
	awful.key({ modkey, 'Shift' }, 'Right', function()
		awful.client.swap.byidx(1)
	end, { description = 'swap with previous client by index', group = 'client' }),
	--- }}}

	-- On the fly useless gaps change {{{

	-- awful.key({ modkey, 'Shift' }, '=', function()
	-- 	awful.tag.setmwfact(0.85)
	-- end, { description = 'increment useless gaps', group = 'tag' }),
	-- awful.key({ modkey }, '-', function()
	-- 	awful.tag.setmwfact(0.50)
	-- end, { description = 'decrement useless gaps', group = 'tag' }),
	--- }}}

	-- Standard program     {{{

	awful.key({ modkey }, 'Return', function()
		awful.spawn('alacritty')
	end, { description = 'open a ' .. terminal, group = 'launcher' }),
	awful.key({ modkey, 'Shift' }, 'Return', function()
		awful.spawn('kitty')
	end, { description = 'open a kitty', group = 'launcher' }),
	awful.key({ modkey }, 'w', function()
		awful.spawn(browser)
	end, { description = 'open a ' .. browser, group = 'launcher' }),
	awful.key({ modkey, 'Shift' }, 'w', function()
		awful.spawn(browser ~= 'firefox' and 'firefox' or 'brave')
	end, { description = 'open a firefox', group = 'launcher' }),
	awful.key({ modkey }, 'e', function()
		awful.spawn('thunar')
	end, { description = 'open a file manager', group = 'launcher' }),
	awful.key({ modkey, 'Shift' }, 'e', function()
		awful.spawn('alacritty -e ranger')
	end, { description = 'open a ranger', group = 'launcher' }),
	--- }}}

	-- User programs {{{
	-- awful.key({ modkey, "Shift" }, "d", function()
	-- 	awful.spawn("rofi -show run -async-pre-read")
	-- end, { description = "Launch Rofi", group = "launcher" }),

	awful.key({ modkey }, 'd', function()
		awful.spawn('dmenu_run_history -i')
	end, { description = 'Launch Dmenu', group = 'launcher' }),

	awful.key({ 'altkey' }, 'Print', function()
		awful.spawn.with_shell('flameshot gui')
	end, { description = 'Capture Screenshot(Fullscreen - flameshot)', group = 'launcher' }),
	awful.key({}, 'Print', function()
		awful.spawn.with_shell('take_ss clip')
	end, { description = 'Capture Screenshot(Fullscreen - Maim)', group = 'launcher' }),
	awful.key({ 'Shift' }, 'Print', function()
		awful.spawn.with_shell('take_ss focus')
	end, { description = 'Capture Screenshot(focus)', group = 'launcher' }),

	awful.key({ modkey }, 'v', function()
		awful.spawn('virt-manager')
	end, { description = 'Launch VirtualBox', group = 'launcher' }),
	awful.key({ modkey }, 'g', function()
		awful.spawn('qalculate-gtk')
	end, { description = 'Launch Calculator', group = 'launcher' }),
	--- }}}

	-- Custom Scripts {{{

	awful.key({ modkey, 'Control' }, 's', function()
		awful.spawn.with_shell('logout_prompt')
	end, { description = 'Ask For Logout(ie shutdown,reboot or logout)', group = 'scripts' }),

	awful.key({ modkey, altkey }, 'c', function()
		awful.spawn('open-rcs')
	end, { description = 'Edit RC files', group = 'scripts' }),
	awful.key({ modkey, altkey }, 'g', function()
		awful.spawn('open-games')
	end, { description = 'Launch Game', group = 'scripts' }),

	awful.key({ 'Control', altkey }, 'p', function()
		awful.spawn('get-class-name')
	end, { description = 'Get window class name', group = 'scripts' }),
	awful.key({ 'Control', altkey }, 'c', function()
		awful.spawn.with_shell('xcolor -s')
	end, { description = 'Color Selector', group = 'scripts' }),

	awful.key({ 'Control', altkey }, 'e', function()
		awful.spawn('rofie')
	end, { description = 'Launch Emoji Selector', group = 'scripts' }),

	awful.key({ 'Control', altkey }, 'v', function()
		awful.spawn('pavucontrol')
	end, { description = 'launch volume controller', group = 'launcher' }),

	-- awful.key({ modkey }, "F12", function()
	-- 	awful.spawn.with_shell("tdrop -am -w 60% -h 55% -x 20% -y 25% st -c 'Minipad'")
	-- end, { description = "launch Dropdown(Scratch pad) Terminal", group = "launcher" }),

	awful.key({ modkey }, 'y', function()
		awful.spawn.with_shell('clipboard')
	end, { description = 'Launch Clipboard Manager', group = 'scripts' }),
	awful.key({ modkey }, 'r', function()
		awful.spawn.with_shell('rofi -show drun -async-pre-read')
	end, { description = 'Rofi Application Launcher', group = 'launcher' }),
	-- awful.key({ modkey }, "z", function()
	-- 	quake:toggle()
	-- end),

	--- }}}

	awful.key({ modkey }, 'F1', hotkeys_popup.show_help, { description = 'show help', group = 'awesome' }),

	-- Standard program

	awful.key({ modkey, 'Control' }, 'r', awesome.restart, { description = 'reload awesome', group = 'awesome' }),
	awful.key({ modkey, 'Control' }, 'x', awesome.quit, { description = 'quit awesome', group = 'awesome' }),
	awful.key({ modkey, 'Control' }, 'l', function()
		awful.spawn(
			string.format(
				'i3lock -k --time-pos "100:1000" --greeter-text="Welcome Back" --wrong-text="Incorrect password!" -i %s/Pictures/Wallpapers/canvas.jpg',
				os.getenv('HOME')
			)
		)
	end, { description = 'i3lock', group = 'awesome' }),

	awful.key({ modkey, 'Control' }, 'n', function()
		local c = awful.client.restore()
		-- Focus restored client
		if c then
			c:emit_signal('request::activate', 'key.unminimize', { raise = true })
		end
	end, { description = 'restore minimized', group = 'client' }),

	-- Prompt
	-- awful.key({ modkey }, 'r', function()
	-- 	awful.screen.focused().mypromptbox:run()
	-- end, { description = 'run prompt', group = 'launcher' }),

	awful.key({ modkey }, 'x', function()
		awful.prompt.run({
			prompt = 'Run Lua code: ',
			textbox = awful.screen.focused().mypromptbox.widget,
			exe_callback = awful.util.eval,
			history_path = awful.util.get_cache_dir() .. '/history_eval',
		})
	end, { description = 'lua execute prompt', group = 'awesome' }),

	-- Brightness   {{{

	awful.key({}, 'XF86MonBrightnessUp', function()
		brightness_widget:inc()
	end, { description = 'Brightness +10%', group = 'hotkeys' }),
	awful.key({}, 'XF86MonBrightnessDown', function()
		brightness_widget:dec()
	end, { description = 'Brightness -10%', group = 'hotkeys' }),
	awful.key({ 'Shift' }, 'XF86MonBrightnessUp', function()
		os.execute('brightnessctl s 200')
	end, { description = '-10%', group = 'hotkeys' }),
	awful.key({ 'Shift' }, 'XF86MonBrightnessDown', function()
		os.execute('brightnessctl s 20')
	end, { description = '-10%', group = 'hotkeys' }),
	--- }}}

	-- Volume Control {{{

	awful.key({}, 'XF86AudioRaiseVolume', function()
		-- os.execute("pamixer -i 10 --allow-boost")
		-- os.execute('pactl set-sink-volume @DEFAULT_SINK@ +10%')
		volume:inc()
		-- beautiful.volume.update()
	end, { description = 'volume up', group = 'hotkeys' }),
	awful.key({}, 'XF86AudioLowerVolume', function()
		-- os.execute("pamixer -d 10 --allow-boost")
		-- os.execute('pactl set-sink-volume @DEFAULT_SINK@ -10%')
		volume:dec()
		-- beautiful.volume.update()
	end, { description = 'volume down', group = 'hotkeys' }),
	awful.key({}, 'XF86AudioMute', function()
		-- os.execute("pamixer --toggle-mute")
		-- os.execute('pactl set-sink-mute @DEFAULT_SINK@ toggle')
		volume:toggle()
		-- beautiful.volume.update()
	end, { description = 'toggle mute', group = 'hotkeys' })

	---  }}}
)

-- Handle Windows(Like toggle floating,move to another tag,etc) {{{
clientkeys = gears.table.join(
	awful.key({ modkey }, 'f', function(c)
		c.fullscreen = not c.fullscreen
		local cur_tag = client.focus and client.focus.first_tag or nil
		if not cur_tag then
			naughty.notify({
				preset = naughty.config.presets.critical,
				title = 'Not Found',
				text = 'Current tag returned nil',
			})
		end
		for _, cls in ipairs(cur_tag:clients()) do
			-- minimize all windows except the focused one
			if c.window ~= cls.window then
				cls.hidden = c.fullscreen
				-- mouse.screen.mywibox.visible = not c.fullscreen
			end
		end
		c:raise()
	end, { description = 'toggle fullscreen', group = 'client' }),
	awful.key({ modkey, 'Shift' }, 'q', function(c)
		c:kill()
	end, { description = 'close', group = 'client' }),
	awful.key({ modkey }, 's', awful.client.floating.toggle, { description = 'toggle floating', group = 'client' }),
	awful.key({ modkey }, 't', function(c)
		awful.layout.set(awful.layout.suit.tile)
	end, { description = 'toggle keep on top', group = 'client' }),
	awful.key({ modkey }, 'n', function(c)
		c.minimized = not c.minimized
	end, { description = 'minimize focused window', group = 'client' }),
	awful.key({ modkey }, 'm', function()
		-- c.maximized = not c.maximized
		-- c:raise()
		local layout_name = awful.layout.get().name
		local change_layout = layout_name == 'max' and awful.layout.suit.tile or awful.layout.suit.max
		awful.layout.set(change_layout)
	end, { description = 'Toggle Between max layout', group = 'client' }),
	awful.key({ modkey, 'Shift' }, 'm', function(c)
		c.maximized = not c.maximized
		c:raise()
	end, { description = 'maximize focused window', group = 'client' }),

	awful.key({ modkey, 'Control' }, 'Return', function(c)
		c:swap(awful.client.getmaster())
	end, { description = 'move to master', group = 'client' }),

	awful.key({ modkey, 'Control' }, 'Up', function(c)
		local move = 20
		if c.floating then
			c:relative_move(0, -move, 0, 0)
		end
	end, { description = 'Move Floating Window towards up', group = 'client' }),

	awful.key({ modkey, 'Control' }, 'Down', function(c)
		local move = 20
		if c.floating then
			c:relative_move(0, move, 0, 0)
		end
	end, { description = 'Move Floating Window towards down', group = 'client' }),
	awful.key({ modkey, 'Control' }, 'Left', function(c)
		local move = 20
		if c.floating then
			c:relative_move(-move, 0, 0, 0)
		end
	end, { description = 'Move Floating Window towards left', group = 'client' }),
	awful.key({ modkey, 'Control' }, 'Right', function(c)
		local move = 20
		if c.floating then
			c:relative_move(move, 0, 0, 0)
		end
	end, { description = 'Move Floating Window towards right', group = 'client' })
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	globalkeys = gears.table.join(
		globalkeys,
		-- View tag only.
		awful.key({ modkey }, '#' .. i + 9, function()
			local screen = awful.screen.focused()
			local tag = screen.tags[i]
			if tag then
				tag:view_only()
			end
		end, { description = 'view tag #' .. i, group = 'tag' }),
		-- Toggle tag display.
		awful.key({ modkey, 'Control' }, '#' .. i + 9, function()
			local screen = awful.screen.focused()
			local tag = screen.tags[i]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end, { description = 'toggle tag #' .. i, group = 'tag' }),
		-- Move client to tag.
		awful.key({ modkey, 'Shift' }, '#' .. i + 9, function()
			if client.focus then
				local tag = client.focus.screen.tags[i]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end, { description = 'move focused client to tag #' .. i, group = 'tag' }),
		-- Toggle tag on focused client.
		awful.key({ modkey, 'Control', 'Shift' }, '#' .. i + 9, function()
			if client.focus then
				local tag = client.focus.screen.tags[i]
				if tag then
					client.focus:toggle_tag(tag)
				end
			end
		end, { description = 'toggle focused client on tag #' .. i, group = 'tag' })
	)
end

clientbuttons = gears.table.join(
	awful.button({}, 1, function(c)
		c:emit_signal('request::activate', 'mouse_click', { raise = true })
	end),
	awful.button({ modkey }, 1, function(c)
		c:emit_signal('request::activate', 'mouse_click', { raise = true })
		awful.mouse.client.move(c)
	end),
	awful.button({ modkey }, 3, function(c)
		c:emit_signal('request::activate', 'mouse_click', { raise = true })
		awful.mouse.client.resize(c)
	end)
)
-- }}}

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
	-- All clients will match this rule.
	{
		rule = {},
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			keys = clientkeys,
			buttons = clientbuttons,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	},

	-- Floating clients.
	{
		rule_any = {
			instance = {
				'DTA', -- Firefox addon DownThemAll.
				'copyq', -- Includes session name in class.
				'pinentry',
			},
			class = {
				'Arandr',
				'Blueman-manager',
				'Gpick',
				'Kruler',
				'MessageWin', -- kalarm.
				'Sxiv',
				'Tor Browser', -- Needs a fixed window size to avoid fingerprinting by screen size.
				'Wpa_gui',
				'veromix',
				'xtightvncviewer',
				'Arcolinux-tweak-tool.py',
				'Arcologout.py',
				'albert',
				'feh',
				'Qalculate-gtk',
				'Galculator',
				'Gnome-calculator',
				'Nitrogen',
				'Grub-customizer',
				'Pavucontrol',
				'Minipad',
				'Evolution-alarm-notify',
				'Connman-gtk',
				'QuakeDD',
			},

			-- Note that the name property shown in xprop might be set slightly after creation of the client
			-- and the name shown there might not match defined rules here.
			name = {
				'Event Tester', -- xev.
			},
			role = {
				'AlarmWindow', -- Thunderbird's calendar.
				'ConfigManager', -- Thunderbird's about:config.
				'pop-up', -- e.g. Google Chrome's (detached) Developer Tools.
			},
		},
		properties = { floating = true },
	},

	-- Add titlebars to normal clients and dialogs
	{ rule_any = { type = { 'normal', 'dialog' } }, properties = { titlebars_enabled = true } },

	{
		rule_any = {
			class = { 'firefox', 'LibreWolf', 'Brave-browser', 'qutebrowser', 'waterfox-current', 'Chromium' },
		},
		properties = { screen = 1, tag = awful.screen.focused().tags[2], switch_to_tags = true },
	},

	{
		rule_any = { class = { 'Steam', 'Lutris', 'Timeshift-gtk' } },
		properties = { floating = true, screen = 1, tag = awful.screen.focused().tags[3], switch_to_tags = true },
	},

	{
		rule_any = { class = { 'Evolution', 'mpv', 'vlc', 'parole' } },
		properties = { screen = 1, tag = awful.screen.focused().tags[4], switch_to_tags = true },
	},

	{
		rule_any = { class = { 'VirtualBox Manager', 'Virt-manager' } },
		properties = { floating = true, screen = 1, tag = awful.screen.focused().tags[6], switch_to_tags = true },
	},
	{
		rule_any = { class = { 'VirtualBox Machine' } },
		properties = { screen = 1, tag = awful.screen.focused().tags[6], switch_to_tags = true },
	},
}
-- }}}

-- {{{ Signals

screen.connect_signal('arrange', function(s)
	local only_one = #s.tiled_clients == 1
	local layout_name = awful.layout.get().name
	for _, c in pairs(s.clients) do
		if (only_one and not c.floating) or layout_name == 'max' then
			c.border_width = 0
		else
			c.border_width = beautiful.border_width
		end
	end
end)
-- Signal function to execute when a new client appears.
client.connect_signal('manage', function(c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	-- if not awesome.startup then awful.client.setslave(c) end

	if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal('request::titlebars', function(c)
	-- buttons for the titlebar
	local buttons = gears.table.join(
		awful.button({}, 1, function()
			c:emit_signal('request::activate', 'titlebar', { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			c:emit_signal('request::activate', 'titlebar', { raise = true })
			awful.mouse.client.resize(c)
		end)
	)

	awful.titlebar(c):setup({
		{ -- Left
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},
		{ -- Middle
			{ -- Title
				align = 'center',
				widget = awful.titlebar.widget.titlewidget(c),
			},
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},
		{ -- Right
			awful.titlebar.widget.floatingbutton(c),
			awful.titlebar.widget.stickybutton(c),
			awful.titlebar.widget.ontopbutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal(),
		},
		layout = wibox.layout.align.horizontal,
	})
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal('mouse::enter', function(c)
	c:emit_signal('request::activate', 'mouse_enter', { raise = false })
end)

client.connect_signal('focus', function(c)
	c.border_color = beautiful.border_focus
end)
client.connect_signal('unfocus', function(c)
	c.border_color = beautiful.border_normal
end)
-- Focus urgent clients automatically
client.connect_signal('property::urgent', function(c)
	local ignore_urgent = { 'teams-for-linux', 'microsoft teams - preview', 'Microsoft Teams - Preview' }
	for _, ignore in ipairs(ignore_urgent) do
		if c.class == ignore then
			return
		end
	end
	c.minimized = false
	c:jump_to()
end)
-- Titlebars only on floating windows {{{
-- Create a titlebar for the client.
-- By default, awful.rules will create one, but all it does is to call this
-- function.

function dynamic_title(c)
	if c.floating or c.first_tag.layout.name == 'floating' then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end

client.connect_signal('property::floating', function(c)
	if c.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end)

tag.connect_signal('property::layout', function(t)
	local clients = t:clients()
	for _, c in pairs(clients) do
		if c.floating or c.first_tag.layout.name == 'floating' then
			awful.titlebar.show(c)
		else
			awful.titlebar.hide(c)
		end
	end
end)

client.connect_signal('manage', dynamic_title)
client.connect_signal('tagged', dynamic_title)
-- }}}
