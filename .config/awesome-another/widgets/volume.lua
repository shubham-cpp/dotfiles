--[[
create an awesomewm volume widget with following requirements:
- use pactl cli
- show icon and text. icon contains nerd icons for different volume levels(a separate icon for mute) and text contains volume level.
- the volume widget should only update on keypress(like XF86AudioRaiseVolume,XF86AudioLowerVolume,XF86AudioMute)
--]]
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")

-- Define the widget icon fonts
local icon_mute = "󰖁 " -- Nerd Fonts: Volume Mute
local icon_low = " " -- Nerd Fonts: Volume Low
local icon_medium = "󰕾 " -- Nerd Fonts: Volume Medium
local icon_high = " " -- Nerd Fonts: Volume High

local M = {}
-- local volume_get_command = "pactl get-sink-volume @DEFAULT_SINK@ | cut -s -d/ -f2,4; pactl get-sink-mute @DEFAULT_SINK@"
local volume_get_command = "wpctl get-volume @DEFAULT_AUDIO_SINK@"
local volume_set_command =
	"wpctl set-mute @DEFAULT_AUDIO_SOURCE@ 0;wpctl set-mute @DEFAULT_AUDIO_SINK@ 0;wpctl set-volume @DEFAULT_AUDIO_SINK@ %s"
local volume_mute_command = "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle;wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

-- Define the volume widget
M.volume_widget = wibox.widget({
	{
		id = "vol_my_icon",
		widget = wibox.widget.textbox,
		font = "FiraCode Nerd Font 14",
		text = icon_high,
	},
	{
		id = "vol_my_text",
		widget = wibox.widget.textbox,
		font = "sans 10",
		text = "100%",
	},
	layout = wibox.layout.fixed.horizontal,
})
local noti = nil
-- Update the widget function
M.update_volume = function(widget)
	awful.spawn.easy_async_with_shell(volume_get_command, function(stdout)
		-- local volume = stdout:match("(%d?%d?%d)%% /")
		-- local mute = stdout:match("Mute: (%a+)")
		local volume = stdout:match("%d+%.%d+")
		local mute = stdout:match("[MUTED]")
		if mute ~= nil then
			widget.vol_my_icon.text = icon_mute
			widget.vol_my_text.text = "Mute"
		elseif volume then
			-- volume = tonumber(string.format("% 3d", volume))
			volume = tonumber(volume) * 100
			if volume < 20 then
				widget.vol_my_icon.text = icon_low
			elseif volume < 70 then
				widget.vol_my_icon.text = icon_medium
			else
				widget.vol_my_icon.text = icon_high
			end
			widget.vol_my_text.text = volume .. "%"
		end
		if noti ~= nil then
			naughty.destroy(noti)
		end
		noti = naughty.notify({
			text = "Volume: " .. widget.vol_my_text.text,
			timeout = 1,
			font = "FiraCode Nerd Font 28",
			preset = naughty.config.presets.low,
		})
	end)
end

-- Update the widget on keypress
awesome.connect_signal("daemon::signal::volume", function()
	M.update_volume(M.volume_widget)
end)

-- Initial update of the widget
M.update_volume(M.volume_widget)

M.volume_increase = function()
	awful.spawn.easy_async_with_shell(string.format(volume_set_command, "10%+"), function()
		M.update_volume(M.volume_widget)
	end)
end

M.volume_decrease = function()
	awful.spawn.easy_async_with_shell(string.format(volume_set_command, "10%-"), function()
		M.update_volume(M.volume_widget)
	end)
end

M.volume_toggle = function()
	awful.spawn.easy_async_with_shell(volume_mute_command, function()
		M.update_volume(M.volume_widget)
	end)
end

M.volume_widget:buttons(gears.table.join(
	awful.button({}, 1, M.volume_toggle),
	awful.button({}, 3, function()
		awful.spawn("pavucontrol")
	end),
	awful.button({}, 4, M.volume_increase),
	awful.button({}, 5, M.volume_decrease)
))

return M
