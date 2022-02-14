-- vim:foldmethod=marker:foldlevelstart=0

-- Required libraries {{{

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type

local gears = require("gears")
-- local ruled         = require("ruled.client")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local lain = require("lain")
--local menubar       = require("menubar")
-- local freedesktop   = require("freedesktop")
-- local hotkeys_popup = require("awful.hotkeys_popup").widget
require("awful.hotkeys_popup.keys")
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
local dpi = require("beautiful.xresources").apply_dpi
local brightness_widget = require("widgets.brightness.brightness")

-- For dropdown like terminal
local quake = lain.util.quake({
	app = "st",
	argname = "-n %s",
	height = 0.6,
	width = 0.5,
	vert = "center",
	horiz = "center",
})

-- }}}

-- Error handling {{{
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({
		preset = naughty.config.presets.critical,
		title = "Oops, there were errors during startup!",
		text = awesome.startup_errors,
	})
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal("debug::error", function(err)
		if in_error then
			return
		end
		in_error = true

		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Oops, an error happened!",
			text = tostring(err),
		})
		in_error = false
	end)
end
-- }}}

-- Autostart windowless processes {{{
--- I dont use this since it slows down startup
--- (My Laptop is very old)
-- awful.spawn.easy_async_with_shell("~/.config/awesome/autorun.sh")
-- This function will run once every time Awesome is started
-- local function run_once(cmd_arr)
--     for _, cmd in ipairs(cmd_arr) do
--         awful.spawn.easy_async_with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
--     end
-- end
-- -- -- entries must be separated by commas
-- run_once({
--     -- "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
--     -- "lxqt-policykit-agent",
--     "xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55",
--     -- "picom -b --experimental-backends",
--     "kitty",
--     "redshift",
--     "greenclip daemon"
-- })

-- }}}

-- Variable definitions {{{

local themes = {
	"blackburn", -- 1
	"copland", -- 2
	"dremora", -- 3
	"holo", -- 4
	"multicolor", -- 5
	"powerarrow", -- 6
	"powerarrow-dark", -- 7
	"rainbow", -- 8
	"steamburn", -- 9
	"vertex", -- 10
}
local chosen_theme = themes[7]
local modkey = "Mod4"
local altkey = "Mod1"
local terminal = os.getenv("TERMINAL") or "xterm"
-- local file_manager = os.getenv("FILE_BROWSER") or "thunar"
local file_manager = "thunar"
local vi_focus = false -- vi-like client focus - https://github.com/lcpz/awesome-copycats/issues/275
-- local cycle_prev   = true -- cycle trough all previous client or just the first -- https://github.com/lcpz/awesome-copycats/issues/274
-- local editor       = os.getenv("EDITOR") or "nano"
local browser = os.getenv("BROWSER") or "firefox"

-- naughty.config.defaults.timeout = 3
-- naughty.config.defaults.border_width = 2
awful.util.terminal = terminal
awful.util.tagnames = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
awful.layout.layouts = {
	awful.layout.suit.tile,
	awful.layout.suit.floating,
	-- awful.layout.suit.spiral.dwindle,
	-- awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	awful.layout.suit.fair,
	--awful.layout.suit.fair.horizontal,
	--awful.layout.suit.spiral,
	awful.layout.suit.max,
	--awful.layout.suit.max.fullscreen,
	--awful.layout.suit.magnifier,
	--awful.layout.suit.corner.nw,
	--awful.layout.suit.corner.ne,
	--awful.layout.suit.corner.sw,
	--awful.layout.suit.corner.se,
	--lain.layout.cascade,
	--lain.layout.cascade.tile,
	lain.layout.centerwork,
	--lain.layout.centerwork.horizontal,
	--lain.layout.termfair,
	--lain.layout.termfair.center,
}
awful.util.taglist_buttons = my_table.join(
	awful.button({}, 1, function(t)
		t:view_only()
	end),
	awful.button({ modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({}, 3, awful.tag.viewtoggle)
	-- awful.button({ modkey }, 3, function(t)
	--     if client.focus then
	--         client.focus:toggle_tag(t)
	--     end
	-- end)
	-- awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
	-- awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

awful.util.tasklist_buttons = my_table.join(
	awful.button({}, 1, function(c)
		if c == client.focus then
			c.minimized = true
		else
			--c:emit_signal("request::activate", "tasklist", {raise = true})<Paste>

			-- Without this, the following
			-- :isvisible() makes no sense
			c.minimized = false
			if not c:isvisible() and c.first_tag then
				c.first_tag:view_only()
			end
			-- This will also un-minimize
			-- the client, if needed
			client.focus = c
			c:raise()
		end
	end),
	awful.button({}, 3, function(c)
		c:kill()
	end)
	-- awful.button({ }, 3, function ()
	--     local instance = nil

	--     return function ()
	--         if instance and instance.wibox.visible then
	--             instance:hide()
	--             instance = nil
	--         else
	--             instance = awful.menu.clients({theme = {width = dpi(250)}})
	--         end
	--     end
	-- end)
	-- awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
	-- awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
)

lain.layout.termfair.nmaster = 3
lain.layout.termfair.ncol = 1
lain.layout.termfair.center.nmaster = 3
lain.layout.termfair.center.ncol = 1
lain.layout.cascade.tile.offset_x = dpi(2)
lain.layout.cascade.tile.offset_y = dpi(32)
lain.layout.cascade.tile.extra_padding = dpi(5)
lain.layout.cascade.tile.nmaster = 5
lain.layout.cascade.tile.ncol = 2

beautiful.init(string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme))

-- }}}

-- Screen {{{
-- No borders when rearranging only 1 non-floating or maximized client
screen.connect_signal("arrange", function(s)
	local only_one = #s.tiled_clients == 1
	local layout_name = awful.layout.get().name
	for _, c in pairs(s.clients) do
		if (only_one and not c.floating) or layout_name == "max" then
			c.border_width = 0
		else
			c.border_width = beautiful.border_width
		end
	end
end)
-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s)
	beautiful.at_screen_connect(s)
end)
-- }}}

-- Mouse bindings {{{
root.buttons(my_table.join(
	awful.button({}, 3, function()
		awful.spawn("rofi -modi drun -show drun -icons")
	end),
	awful.button({}, 4, awful.tag.viewnext),
	awful.button({}, 5, awful.tag.viewprev)
))
-- }}}

-- Key bindings {{{

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
	if direction == "down" or direction == "left" then
		pixel = -pixel
	end

	if direction == "left" or direction == "right" then
		awful.tag.incmwfact(pixel)
	else
		local master_win = awful.client.getmaster().window
		local current_win = client.focus.window
		if master_win == current_win then
			awful.tag.incmwfact(pixel)
		else
			pixel = direction == "up" and -0.05 or 0.05
			awful.client.incwfact(pixel)
		end
	end
end

local resize_float = function(c, direction, move)
	local move = move or 20
	local direction = direction or "up"
	local c = c or client.focus
	if direction == "left" then
		c:relative_move(0, 0, -move, 0)
	elseif direction == "right" then
		c:relative_move(0, 0, move, 0)
	elseif direction == "down" then
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

globalkeys = my_table.join(

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

	-- awful.key({ modkey }, "[", awful.tag.viewprev, { description = "view previous", group = "tag" }),
	-- awful.key({ modkey }, "]", awful.tag.viewnext, { description = "view next", group = "tag" }),
	awful.key({ modkey, "Shift" }, "[", function()
		move_to_tag(-1)
	end, { description = "move the focused client to previous tag", group = "tag" }),
	awful.key({ modkey, "Shift" }, "]", function()
		move_to_tag(1)
	end, { description = "move the focused client to next tag", group = "tag" }),
	awful.key({ modkey }, "Tab", function()
		awful.tag.history.restore()
		if client.focus then
			client.focus:raise()
		end
	end, { description = "go back", group = "tag" }),

	--- }}}

	-- Non-empty tag browsing {{{
	awful.key({ modkey }, "grave", function()
		tag_view_nonempty(1)
	end, { description = "view next nonempty", group = "tag" }),

	awful.key({ modkey, "Shift" }, "grave", function()
		tag_view_nonempty(-1)
	end, { description = "view  previous nonempty", group = "tag" }),

	awful.key({ modkey }, "comma", function()
		tag_view_nonempty(-1)
	end, { description = "view  previous nonempty", group = "tag" }),

	awful.key({ modkey }, "period", function()
		tag_view_nonempty(1)
	end, { description = "view next nonempty", group = "tag" }),
	--- }}}

	-- By direction client focus    {{{

	awful.key({ modkey }, "Down", function()
		awful.client.focus.global_bydirection("down")
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus down", group = "client" }),
	awful.key({ modkey }, "Up", function()
		awful.client.focus.global_bydirection("up")
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus up", group = "client" }),
	awful.key({ modkey }, "Left", function()
		awful.client.focus.global_bydirection("left")
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus left", group = "client" }),
	awful.key({ modkey }, "Right", function()
		awful.client.focus.global_bydirection("right")
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus right", group = "client" }),
	awful.key({ modkey }, "j", function()
		awful.client.focus.byidx(1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus down", group = "client" }),
	awful.key({ modkey }, "k", function()
		awful.client.focus.byidx(-1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus up", group = "client" }),
	awful.key({ modkey }, "h", function()
		awful.client.focus.global_bydirection("left")
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus left", group = "client" }),
	awful.key({ modkey }, "l", function()
		awful.client.focus.global_bydirection("right")
		if client.focus then
			client.focus:raise()
		end
	end, { description = "focus right", group = "client" }),
	awful.key({ altkey }, "Tab", function()
		awful.client.focus.byidx(1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = "Focus to next Client", group = "client" }),
	awful.key({ altkey, "Shift" }, "Tab", function()
		awful.client.focus.byidx(-1)
		if client.focus then
			client.focus:raise()
		end
	end, { description = "Focus to previous Client", group = "client" }),
	-- }}}

	-- Layout manipulation {{{

	awful.key({ modkey, "Shift" }, "j", function()
		awful.client.swap.byidx(1)
	end, { description = "swap with next client by index", group = "client" }),
	awful.key({ modkey, "Shift" }, "k", function()
		awful.client.swap.byidx(-1)
	end, { description = "swap with previous client by index", group = "client" }),
	awful.key({ modkey, "Shift" }, "Left", function()
		awful.client.swap.byidx(-1)
	end, { description = "swap with next client by index", group = "client" }),
	awful.key({ modkey, "Shift" }, "Right", function()
		awful.client.swap.byidx(1)
	end, { description = "swap with previous client by index", group = "client" }),
	--- }}}

	-- On the fly useless gaps change {{{

	awful.key({ modkey, "Shift" }, "=", function()
		lain.util.useless_gaps_resize(1)
	end, { description = "increment useless gaps", group = "tag" }),
	awful.key({ modkey }, "-", function()
		lain.util.useless_gaps_resize(-1)
	end, { description = "decrement useless gaps", group = "tag" }),
	--- }}}

	-- Dynamic tagging NOT USING RIGHT NOW{{{
	-- awful.key({ modkey, "Shift" }, "n", function () lain.util.add_tag() end,
	--           {description = "add new tag", group = "tag"}),
	-- awful.key({ modkey, "Shift" }, "r", function () lain.util.rename_tag() end,
	--           {description = "rename tag", group = "tag"}),
	-- awful.key({ modkey, "Shift" }, "Left", function () lain.util.move_tag(-1) end,
	--           {description = "move tag to the left", group = "tag"}),
	-- awful.key({ modkey, "Shift" }, "Right", function () lain.util.move_tag(1) end,
	--           {description = "move tag to the right", group = "tag"}),
	-- awful.key({ modkey, "Shift" }, "d", function () lain.util.delete_tag() end,
	--           {description = "delete tag", group = "tag"}),
	--- }}}

	-- Standard program     {{{

	-- awful.key({ modkey }, "Return", function()
	-- 	awful.spawn(terminal)
	-- end, { description = "open " .. terminal, group = "launcher" }),
	-- awful.key({ modkey, "Shift" }, "Return", function()
	-- 	awful.spawn("kitty")
	-- end, { description = "open kitty", group = "launcher" }),
	-- awful.key({ modkey }, "KP_Enter", function()
	-- 	awful.spawn("alacritty")
	-- end, { description = "open alacritty", group = "launcher" }),
	-- awful.key({ modkey }, "KP_End", function()
	-- 	awful.spawn("xterm")
	-- end, { description = "open xterm", group = "launcher" }),

	-- awful.key({ modkey }, "e", function()
	-- 	awful.spawn(file_manager)
	-- end, { description = "open Default Filemanager", group = "launcher" }),
	-- awful.key({ modkey, "Shift" }, "e", function()
	-- 	awful.spawn(terminal .. " -e vifmrun")
	-- end, { description = string.format("open a vifm inside %s", os.getenv("TERMINAL")), group = "launcher" }),
	--- }}}

	-- User programs {{{
	-- awful.key({ modkey }, "w", function()
	-- 	awful.spawn("firefox")
	-- end, { description = "run " .. browser, group = "launcher" }),
	-- awful.key({ modkey, "Shift" }, "w", function()
	-- 	awful.spawn("brave")
	-- end, { description = "run firefox", group = "launcher" }),

	-- awful.key({ modkey, "Shift" }, "d", function()
	-- 	awful.spawn("rofi -show run -async-pre-read")
	-- end, { description = "Launch Rofi", group = "launcher" }),

	-- awful.key({ modkey }, "d", function()
	-- 	awful.spawn("dmenu_run_history -i")
	-- end, { description = "Launch Dmenu", group = "launcher" }),

	-- awful.key({}, "Print", function()
	-- 	awful.spawn.with_shell("take_ss full")
	-- 	naughty.notify({
	-- 		preset = naughty.config.presets.normal,
	-- 		title = "Screenshot",
	-- 		timeout = 2,
	-- 		text = "Taken Successfully(FULL)",
	-- 	})
	-- end, { description = "Capture Screenshot(Fullscreen)", group = "launcher" }),
	-- awful.key({ "Shift" }, "Print", function()
	-- 	awful.spawn.with_shell("take_ss focus")
	-- 	naughty.notify({
	-- 		preset = naughty.config.presets.normal,
	-- 		title = "Screenshot",
	-- 		timeout = 2,
	-- 		text = "Taken Successfully(Focused Window)",
	-- 	})
	-- end, { description = "Capture Screenshot(Focused Window)", group = "launcher" }),

	-- awful.key({ modkey }, "v", function()
	-- 	awful.spawn("virt-manager")
	-- end, { description = "Launch VirtualBox", group = "launcher" }),
	-- awful.key({ modkey }, "g", function()
	-- 	awful.spawn("qalculate-gtk")
	-- end, { description = "Launch Calculator", group = "launcher" }),
	--- }}}

	-- Custom Scripts {{{

	-- awful.key({ modkey, "Control" }, "s", function()
	-- 	awful.spawn.with_shell("logout_prompt")
	-- end, { description = "Ask For Logout(ie shutdown,reboot or logout)", group = "scripts" }),

	-- awful.key({ modkey, altkey }, "c", function()
	-- 	awful.spawn("open-rcs")
	-- end, { description = "Edit RC files", group = "scripts" }),
	-- awful.key({ modkey, altkey }, "g", function()
	-- 	awful.spawn("open-games")
	-- end, { description = "Launch Game", group = "scripts" }),

	-- awful.key({ "Control", altkey }, "p", function()
	-- 	awful.spawn("get-class-name")
	-- end, { description = "Get window class name", group = "scripts" }),
	-- awful.key({ "Control", altkey }, "c", function()
	-- 	awful.spawn.with_shell("xcolor -s")
	-- end, { description = "Color Selector", group = "scripts" }),

	-- awful.key({ "Control", altkey }, "e", function()
	-- 	awful.spawn("rofie")
	-- end, { description = "Launch Emoji Selector", group = "scripts" }),

	-- awful.key({ "Control", altkey }, "v", function()
	-- 	awful.spawn("pavucontrol")
	-- end, { description = "launch volume controller", group = "launcher" }),

	-- awful.key({ modkey }, "F12", function()
	-- 	awful.spawn.with_shell("tdrop -am -w 60% -h 55% -x 20% -y 25% st -c 'Minipad'")
	-- end, { description = "launch Dropdown(Scratch pad) Terminal", group = "launcher" }),

	-- awful.key({ modkey }, "p", function()
	-- 	awful.spawn("superp")
	-- end, { description = "Launch Emoji Selector", group = "scripts" }),
	-- awful.key({ modkey }, "y", function()
	-- 	awful.spawn.with_shell("clipboard")
	-- end, { description = "Launch Clipboard Manager", group = "scripts" }),
	-- awful.key({ modkey }, "r", function()
	-- 	awful.spawn.with_shell("rofi -show drun -async-pre-read")
	-- end, { description = "Rofi Application Launcher", group = "launcher" }),
	awful.key({ modkey }, "z", function()
		quake:toggle()
	end),

	--- }}}

	-- Specific to awesome window manager {{{
	awful.key({ modkey, "Control" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
	-- awful.key({ modkey }, "Escape", awesome.restart, { description = "reload awesome", group = "awesome" }),
	awful.key({ modkey, "Control" }, "x", awesome.quit, { description = "quit awesome", group = "awesome" }),

	awful.key({ modkey, "Control" }, "n", function()
		local c = awful.client.restore()
		-- Focus restored client
		if c then
			client.focus = c
			c:raise()
		end
	end, { description = "restore minimized", group = "client" }),

	awful.key({ modkey }, "b", function()
		for s in screen do
			s.mywibox.visible = not s.mywibox.visible
			if s.mybottomwibox then
				s.mybottomwibox.visible = not s.mybottomwibox.visible
			end
		end
	end, { description = "toggle wibox", group = "awesome" }),

	--- }}}

	-- Adjust window sizes {{{

	awful.key({ modkey, altkey }, "Down", function()
		resize_window("down")
	end, { description = "Resize titled windows(Down)", group = "client" }),

	awful.key({ modkey, altkey }, "Up", function()
		resize_window("up")
	end, { description = "Resize titled windows(Up)", group = "client" }),

	awful.key({ modkey, altkey }, "Left", function()
		resize_window("left")
	end, { description = "Resize titled windows(Left)", group = "client" }),

	awful.key({ modkey, altkey }, "Right", function()
		resize_window("right")
	end, { description = "Resize titled windows(Right)", group = "client" }),

	awful.key({ modkey }, "space", function()
		awful.layout.inc(1)
	end, { description = "select next", group = "layout" }),
	awful.key({ modkey, "Shift" }, "space", function()
		awful.layout.inc(-1)
	end, { description = "select previous", group = "layout" }),

	--- }}}

	-- Brightness   {{{

	awful.key({}, "XF86MonBrightnessUp", function()
		brightness_widget:inc()
	end, { description = "Brightness +10%", group = "hotkeys" }),
	awful.key({}, "XF86MonBrightnessDown", function()
		brightness_widget:dec()
	end, { description = "Brightness -10%", group = "hotkeys" }),
	awful.key({ "Shift" }, "XF86MonBrightnessUp", function()
		os.execute("brightnessctl s 200")
	end, { description = "-10%", group = "hotkeys" }),
	awful.key({ "Shift" }, "XF86MonBrightnessDown", function()
		os.execute("brightnessctl s 20")
	end, { description = "-10%", group = "hotkeys" }),
	--- }}}

	-- Volume Control {{{

	awful.key({}, "XF86AudioRaiseVolume", function()
		-- os.execute("pamixer -i 10 --allow-boost")
		os.execute("pactl set-sink-volume @DEFAULT_SINK@ +10%")
		beautiful.volume.update()
	end, { description = "volume up", group = "hotkeys" }),
	awful.key({}, "XF86AudioLowerVolume", function()
		-- os.execute("pamixer -d 10 --allow-boost")
		os.execute("pactl set-sink-volume @DEFAULT_SINK@ -10%")
		beautiful.volume.update()
	end, { description = "volume down", group = "hotkeys" }),
	awful.key({}, "XF86AudioMute", function()
		-- os.execute("pamixer --toggle-mute")
		os.execute("pactl set-sink-mute @DEFAULT_SINK@ toggle")
		beautiful.volume.update()
	end, { description = "toggle mute", group = "hotkeys" }),

	---  }}}

	-- Default {{{
	awful.key({ modkey }, "x", function()
		awful.prompt.run({
			prompt = "Run Lua code: ",
			textbox = awful.screen.focused().mypromptbox.widget,
			exe_callback = awful.util.eval,
			history_path = awful.util.get_cache_dir() .. "/history_eval",
		})
	end, { description = "lua execute prompt", group = "awesome" })
)
-- }}}

-- Handle Windows(Like toggle floating,move to another tag,etc) {{{

clientkeys = my_table.join(
	awful.key({ altkey, "Shift" }, "m", lain.util.magnify_client, { description = "magnify client", group = "client" }),
	awful.key({ modkey }, "f", function(c)
		c.fullscreen = not c.fullscreen
		local cur_tag = client.focus and client.focus.first_tag or nil
		for _, cls in ipairs(cur_tag:clients()) do
			-- minimize all windows except the focused one
			if c.window ~= cls.window then
				cls.hidden = c.fullscreen
				-- mouse.screen.mywibox.visible = not c.fullscreen
			end
		end
		c:raise()
	end, { description = "toggle fullscreen", group = "client" }),

	awful.key({ modkey, "Shift" }, "q", function(c)
		c:kill()
	end, { description = "close", group = "client" }),

	awful.key({ modkey }, "s", awful.client.floating.toggle, { description = "toggle floating", group = "client" }),
	awful.key({ modkey }, "t", function(c)
		c.ontop = not c.ontop
	end, { description = "toggle keep on top", group = "client" }),
	awful.key({ modkey }, "n", function(c)
		c.minimized = not c.minimized
	end, { description = "minimize focused window", group = "client" }),
	awful.key({ modkey }, "m", function()
		-- c.maximized = not c.maximized
		-- c:raise()
		local layout_name = awful.layout.get().name
		local change_layout = layout_name == "max" and awful.layout.suit.tile or awful.layout.suit.max
		awful.layout.set(change_layout)
	end, { description = "Toggle Between max layout", group = "client" }),
	awful.key({ modkey, "Shift" }, "m", function(c)
		c.maximized = not c.maximized
		c:raise()
	end, { description = "maximize focused window", group = "client" }),

	awful.key({ modkey, "Control" }, "Return", function(c)
		c:swap(awful.client.getmaster())
	end, { description = "move to master", group = "client" }),

	awful.key({ modkey, "Control" }, "Up", function(c)
		local move = 20
		if c.floating then
			c:relative_move(0, -move, 0, 0)
		end
	end, { description = "Move Floating Window towards up", group = "client" }),

	awful.key({ modkey, "Control" }, "Down", function(c)
		local move = 20
		if c.floating then
			c:relative_move(0, move, 0, 0)
		end
	end, { description = "Move Floating Window towards down", group = "client" }),
	awful.key({ modkey, "Control" }, "Left", function(c)
		local move = 20
		if c.floating then
			c:relative_move(-move, 0, 0, 0)
		end
	end, { description = "Move Floating Window towards left", group = "client" }),
	awful.key({ modkey, "Control" }, "Right", function(c)
		local move = 20
		if c.floating then
			c:relative_move(move, 0, 0, 0)
		end
	end, { description = "Move Floating Window towards right", group = "client" })
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	-- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
	local descr_view, descr_toggle, descr_move, descr_toggle_focus
	if i == 1 or i == 9 then
		descr_view = { description = "view tag #", group = "tag" }
		descr_toggle = { description = "toggle tag #", group = "tag" }
		descr_move = { description = "move focused client to tag #", group = "tag" }
		descr_toggle_focus = { description = "toggle focused client on tag #", group = "tag" }
	end
	globalkeys = my_table.join(
		globalkeys,
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9, function()
			local tag = awful.screen.focused().tags[i]
			if tag then
				tag:view_only()
			end
		end, descr_view),
		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9, function()
			local tag = awful.screen.focused().tags[i]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end, descr_toggle),
		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9, function()
			if client.focus then
				local tag = client.focus.screen.tags[i]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end, descr_move),
		-- Toggle tag on focused client.
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function()
			if client.focus then
				local tag = client.focus.screen.tags[i]
				if tag then
					client.focus:toggle_tag(tag)
				end
			end
		end, descr_toggle_focus)
	)
end

clientbuttons = gears.table.join(
	awful.button({}, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
	end),
	awful.button({ modkey }, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
		awful.mouse.client.move(c)
	end),
	awful.button({ modkey }, 3, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
		awful.mouse.client.resize(c)
	end)
)

-- }}}

-- Set keys
root.keys(globalkeys)
-- }}}

-- Rules {{{
-- Rules to apply to new clients (through the "manage" signal).
-- beautiful.useless_gap = 2

awful.rules.rules = {

	-- All clients will match this rule. {{{
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
			size_hints_honor = false,
		},
	},
	--- }}}

	-- Titlebars
	{ rule_any = { type = { "dialog", "normal" } }, properties = { titlebars_enabled = true } },
	{
		rule_any = {
			class = { "firefox", "LibreWolf", "Brave-browser", "qutebrowser", "waterfox-current", "Chromium" },
		},
		properties = { screen = 1, tag = awful.util.tagnames[2], switch_to_tags = true },
	},

	{
		rule_any = { class = { "Steam", "Lutris", "Timeshift-gtk" } },
		properties = { floating = true, screen = 1, tag = awful.util.tagnames[3], switch_to_tags = true },
	},

	{
		rule_any = { class = { "Evolution", "mpv", "vlc", "parole" } },
		properties = { screen = 1, tag = awful.util.tagnames[4], switch_to_tags = true },
	},

	{
		rule_any = { class = { "VirtualBox Manager", "Virt-manager" } },
		properties = { floating = true, screen = 1, tag = awful.util.tagnames[6], switch_to_tags = true },
	},
	{
		rule_any = { class = { "VirtualBox Machine" } },
		properties = { screen = 1, tag = awful.util.tagnames[6], switch_to_tags = true },
	},

	--- Floating Rules {{{
	{
		rule_any = {
			class = {
				"Arandr",
				"Arcolinux-tweak-tool.py",
				"Arcologout.py",
				"albert",
				"feh",
				"Qalculate-gtk",
				"Galculator",
				"Gnome-calculator",
				"Nitrogen",
				"Grub-customizer",
				"Pavucontrol",
				"Minipad",
				"Evolution-alarm-notify",
				"Connman-gtk",
				"QuakeDD",
			},
		},
		properties = { floating = true, placement = awful.placement.centered, width = 500, height = 480 },
	},

	{
		rule = { class = "rofi" },
		properties = { floating = true, placement = awful.placement.centered, width = 300, height = 350 },
	},
}
-- }}}

-- Signals {{{
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	-- if not awesome.startup then awful.client.setslave(c) end

	if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
	-- Custom
	if beautiful.titlebar_fun then
		beautiful.titlebar_fun(c)
		return
	end

	-- Default
	-- buttons for the titlebar
	local buttons = my_table.join(
		awful.button({}, 1, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({}, 2, function()
			c:kill()
		end),
		awful.button({}, 3, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)

	awful.titlebar(c, { size = dpi(16) }):setup({
		{ -- Left
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},
		{ -- Middle
			{ -- Title
				align = "center",
				widget = awful.titlebar.widget.titlewidget(c),
			},
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},
		{ -- Right
			-- awful.titlebar.widget.floatingbutton (c),
			awful.titlebar.widget.minimizebutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			-- awful.titlebar.widget.stickybutton   (c),
			-- awful.titlebar.widget.ontopbutton    (c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal(),
		},
		layout = wibox.layout.align.horizontal,
	})
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
	c:emit_signal("request::activate", "mouse_enter", { raise = vi_focus })
end)

client.connect_signal("focus", function(c)
	c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c)
	c.border_color = beautiful.border_normal
end)

-- Remember Floating geometry {{{
--- Work in progress
-- local floatgeoms = {}

-- tag.connect_signal("property::layout", function(t)
--     for _, c in ipairs(t:clients()) do
--         if ((awful.layout.get(mouse.screen) == awful.layout.suit.floating) or c.floating) then
--             c:geometry(floatgeoms[c.window])
--         end
--     end
--     client.connect_signal("unmanage", function(c) floatgeoms[c.window] = nil end)
-- end)

-- client.connect_signal("property::geometry", function(c)
--     if ((awful.layout.get(mouse.screen) == awful.layout.suit.floating) or c.floating) then
--         floatgeoms[c.window] = c:geometry()
--         -- naughty.notify({ preset = naughty.config.presets.normal,
--         --              title = "Yes its saved",
--         --              text = tostring(floatgeoms[c.window].x) })
--     end
-- end)

-- client.connect_signal("unmanage", function(c) floatgeoms[c.window] = nil end)

-- client.connect_signal("manage", function(c)
--     if ((awful.layout.get(mouse.screen) == awful.layout.suit.floating) or c.floating) then
--         floatgeoms[c.window] = c:geometry()
--     end
-- end)
-- }}}

-- Titlebars only on floating windows {{{
-- Create a titlebar for the client.
-- By default, awful.rules will create one, but all it does is to call this
-- function.

function dynamic_title(c)
	if c.floating or c.first_tag.layout.name == "floating" then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end

client.connect_signal("property::floating", function(c)
	if c.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end)

tag.connect_signal("property::layout", function(t)
	local clients = t:clients()
	for _, c in pairs(clients) do
		if c.floating or c.first_tag.layout.name == "floating" then
			awful.titlebar.show(c)
		else
			awful.titlebar.hide(c)
		end
	end
end)

client.connect_signal("manage", dynamic_title)
client.connect_signal("tagged", dynamic_title)

-- }}}

beautiful.gap_single_client = false
-- }}}
