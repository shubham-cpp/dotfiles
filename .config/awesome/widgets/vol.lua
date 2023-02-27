local awful = require 'awful'
local wibox = require 'wibox'
local watch = require 'awful.widget.watch'
local spawn = require 'awful.spawn'
local naughty = require 'naughty'
local beautiful = require 'beautiful'
local gears = require 'gears'
local gfs = gears.filesystem
local xresources = require 'beautiful.xresources'
local dpi = xresources.apply_dpi
local capture = require('utils.helper').capture
require 'math'
local string = 'string'
local get_volume_cmd
local set_volume_cmd
local inc_volume_cmd
local dec_volume_cmd
local toggle_volume_cmd
local vol_widget = {}
local icon_path = gfs.get_xdg_data_home() .. 'icons/Papirus-Dark/symbolic/status/audio-volume-high-symbolic.svg'

-- local popup_icon = wibox.widget {
--    widget = wibox.widget.imagebox,
--    forced_height = dpi(26),
--    forced_width = dpi(26),
-- }

-- local popup_slider = wibox({
--    screen = awful.screen.focused(),
--    width = dpi(256),
--    height = dpi(56),
--    bg = beautiful.hud_popup_bg,
--    shape = gears.shape.rounded_bar,
--    border_width = beautiful.hud_popup_border_width,
--    border_color = beautiful.hud_popup_border_color,
--    visible = false,
--    ontop = true
-- })

local popup_progressbar = wibox.widget({
  widget = wibox.widget.progressbar,
  shape = gears.shape.rounded_bar,
  color = beautiful.hud_slider_fg or '#d86878',
  background_color = beautiful.hud_slider_bg or '#1a1a1a',
  max_value = 100,
  value = 0,
})

local function show_popup(message)
  -- naughty.notifications = popup_progressbar
  naughty.notify({
    preset = naughty.config.presets.low,
    title = 'Volume',
    font = 'Iosevka Nerd Font 12',
    icon = icon_path,
    replaces_id = 1,
    -- shape = function(cr, w, h)
    -- 	return gears.shape.rounded_rect(cr, tonumber(message.level) * 2, dpi(36), dpi(8))
    -- end,
    text = message.text .. (message.level and string.format('%s', message.level) or ' '),
  })
end

local function get_icon(level, icons)
  if string.len(capture 'pactl get-sink-mute @DEFAULT_SINK@ | grep yes' or '') > 0 then
    return icons[1]
  end
  local v = level or 0
  if v > 90 then
    return icons[5]
  elseif v > 50 then
    return icons[4]
  elseif v > 20 then
    return icons[3]
  else
    return icons[2]
  end
end

local function worker(user_args)
  local args = user_args or {}
  local timeout = 120 -- 2mins
  local program = args.program or 'pactl'
  local step = args.step or 5
  local current_level = 0
  if program == 'pamixer' then
    get_volume_cmd = 'pamixer --get-volume-human'
    set_volume_cmd = 'pamixer --set-volume %s' -- <level>
    inc_volume_cmd = 'pamixer -i ' .. step
    dec_volume_cmd = 'pamixer -d ' .. step
    toggle_volume_cmd = 'pamixer -t'
  elseif program == 'pactl' then
    get_volume_cmd = 'sb-volume' -- "pactl get-sink-volume @DEFAULT_SINK@ | awk -F/ 'NR==1{print $2}' "
    set_volume_cmd = 'pactl set-sink-volume @DEFAULT_SINK@ %s' -- <level>
    inc_volume_cmd = 'pactl set-sink-volume @DEFAULT_SINK@ +' .. step .. '%'
    dec_volume_cmd = 'pactl set-sink-volume @DEFAULT_SINK@ -' .. step .. '%'
    toggle_volume_cmd = 'pactl set-sink-mute @DEFAULT_SINK@ toggle'
  end
  local val = spawn.easy_async_with_shell(get_volume_cmd, function(out)
    return out
  end)
  vol_widget.widget = wibox.widget({
    {
      id = 'vol_txt_icon',
      widget = wibox.widget.textbox,
      markup = val,
      font = 'Hack Nerd Font 14',
    },
    {
      id = 'vol_txt',
      widget = wibox.widget.textbox,
      markup = val,
    },
    spacing = 4,
    layout = wibox.layout.fixed.horizontal,
    set_value = function(self, level)
      -- naughty.notify({
      --   preset = naughty.config.presets.low,
      --   title = 'Volume',
      --   font = 'Iosevka Nerd Font 12',
      --   icon = icon_path,
      --   replaces_id = 1,
      --   text = level,
      -- })
      -- local display_text = get_icon(
      --       tonumber(string.gsub(level, '[^0-9]', '')),
      --       { '婢', '奄', ' ', '墳 ', '  ' }
      --     ) .. '  ' .. string.gsub(level, '[^0-9]', '')
      local display_text =
          get_icon(tonumber(string.gsub(level, '[^0-9]', '')), { '婢', '奄', ' ', '󰕾 ', ' ' })
      self:get_children_by_id('vol_txt_icon')[1]:set_markup_silently(display_text)
      self:get_children_by_id('vol_txt')[1]:set_markup_silently(string.gsub(level, '[^0-9]', ''))
      -- show_popup(level)
    end,
  })
  local update_widget = function(widget, stdout, _, _, _)
    -- local brightness_level = tonumber(string.format('%.0f', stdout))
    current_level = 'pamixer --get-volume-human'
    -- show_popup({ level = 10, text = stdout })
    widget:set_value(stdout)
  end

  function vol_widget:set(value)
    current_level = value
    spawn.easy_async_with_shell(set_volume_cmd, function()
      spawn.easy_async_with_shell(get_volume_cmd, function(out)
        update_widget(vol_widget.widget, out)
        -- show_popup({ level = 10, text = 'From set\n' .. out })

        -- naughty.notify({
        --   preset = naughty.config.presets.low,
        --   title = 'Volume',
        --   font = 'Iosevka Nerd Font 12',
        --   icon = icon_path,
        --   replaces_id = 1,
        --   text = out,
        -- })
        show_popup({ level = out, text = 'Volume level: ' })
      end)
    end)
  end

  function vol_widget:toggle()
    spawn.easy_async_with_shell(toggle_volume_cmd, function()
      spawn.easy_async_with_shell(get_volume_cmd, function(out)
        update_widget(vol_widget.widget, out)
        show_popup({ text = 'Volume: ' .. out })
      end)
    end)
  end

  function vol_widget:inc()
    spawn.easy_async_with_shell(inc_volume_cmd, function()
      spawn.easy_async_with_shell(get_volume_cmd, function(out, err, reason, code)
        update_widget(vol_widget.widget, out)
        show_popup({ level = out, text = 'Volume level: ' })

        popup_progressbar:set_value(tonumber(string.sub(out, 1, 2)))
        -- popup_progressbar.visible = true
      end)
    end)
  end

  function vol_widget:dec()
    spawn.easy_async_with_shell(dec_volume_cmd, function()
      spawn.easy_async_with_shell(get_volume_cmd, function(out)
        update_widget(vol_widget.widget, out)

        show_popup({ level = out, text = 'Volume level: ' })
        popup_progressbar:set_value(tonumber(string.sub(out, 1, 2)))
      end)
    end)
  end

  vol_widget.widget:buttons(awful.util.table.join(
    awful.button({}, 1, function()
      vol_widget:set(20)
    end),
    -- awful.button({}, 3, function()
    -- 	vol_widget:toggle()
    -- end),
    awful.button({}, 4, function()
      vol_widget:inc()
    end),
    awful.button({}, 5, function()
      vol_widget:dec()
    end)
  ))
  watch(get_volume_cmd, timeout, update_widget, vol_widget.widget)
  awful.tooltip({
    objects = { vol_widget.widget },
    timer_function = function()
      return current_level -- .. tonumber(current_level) and ' %' or ' '
    end,
  })
  return vol_widget.widget
end

return setmetatable(vol_widget, {
  __call = function(_, ...)
    return worker(...)
  end,
})

-- return Volume
