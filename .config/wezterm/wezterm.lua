-- Pull in the wezterm API
local wezterm = require "wezterm"
local session_manager = require "session-manager"

-- This table will hold the configuration.
local config = {}
local keys = {}
-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end
-- For example, changing the color scheme:
config.color_scheme = "Tokyo Night"

config.font_size = 11.5
config.font = wezterm.font_with_fallback({
  "FiraCode Nerd Font",
  "JetBrainsMono Nerd Font",
  "Symbols Nerd Font Mono",
})
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }

-- default is true, has more "native" look
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.window_background_opacity = 0.90
-- I don't like putting anything at the ege if I can help it.
config.enable_scroll_bar = false
config.check_for_updates = false
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.tab_bar_at_bottom = true
config.freetype_load_target = "Light"

wezterm.on("save_session", function(window)
  session_manager.save_state(window)
end)
wezterm.on("load_session", function(window)
  session_manager.load_state(window)
end)
wezterm.on("restore_session", function(window)
  session_manager.restore_state(window)
end)

table.insert(keys, {
  key = "s",
  mods = "LEADER",
  action = wezterm.action({ EmitEvent = "save_session" }),
})
table.insert(keys, {
  key = "r",
  mods = "LEADER",
  action = wezterm.action({ EmitEvent = "restore_session" }),
})
table.insert(keys, {
  key = "l",
  mods = "LEADER",
  action = wezterm.action({ EmitEvent = "load_session" }),
})

table.insert(keys, {
  key = "s",
  mods = "LEADER|CTRL",
  action = wezterm.action.SendKey({ key = "s", mods = "CTRL" }),
})
table.insert(keys, {
  key = "j",
  mods = "LEADER",
  action = wezterm.action.ActivatePaneDirection "Down",
})
table.insert(keys, {
  key = "k",
  mods = "LEADER",
  action = wezterm.action.ActivatePaneDirection "Up",
})
table.insert(keys, {
  key = "l",
  mods = "LEADER",
  action = wezterm.action.ActivatePaneDirection "Right",
})
table.insert(keys, {
  key = "h",
  mods = "LEADER",
  action = wezterm.action.ActivatePaneDirection "Left",
})
table.insert(keys, {
  key = "l",
  mods = "CTRL",
  action = wezterm.action.DisableDefaultAssignment,
})
table.insert(keys, {
  key = "z",
  mods = "LEADER",
  action = wezterm.action.TogglePaneZoomState,
})
table.insert(keys, {
  key = "t",
  mods = "LEADER",
  action = wezterm.action.SpawnTab "CurrentPaneDomain",
})
table.insert(keys, {
  key = "v",
  mods = "LEADER",
  action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
})
table.insert(keys, {
  key = "x",
  mods = "LEADER",
  action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
})
table.insert(keys, {
  key = "Enter",
  mods = "CTRL|SHIFT",
  action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
})
table.insert(keys, {
  key = "Backspace",
  mods = "CTRL|SHIFT",
  action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
})
table.insert(keys, {
  key = "c",
  mods = "CTRL|SHIFT",
  action = wezterm.action.CopyTo "ClipboardAndPrimarySelection",
})
table.insert(keys, {
  key = "v",
  mods = "CTRL|SHIFT",
  action = wezterm.action.PasteFrom "Clipboard",
})
table.insert(keys, {
  key = "t",
  mods = "SHIFT|CTRL",
  action = wezterm.action.SpawnTab "CurrentPaneDomain",
})
table.insert(keys, {
  key = "w",
  mods = "SHIFT|CTRL",
  action = wezterm.action.CloseCurrentTab({ confirm = false }),
})

-- local directions = { "Left", "Right", "Down", "Up" }

-- for _, dir in ipairs(directions) do
--   table.insert(keys, {
--     key = dir .. "Arrow",
--     mods = "CTRL|SHIFT",
--     action = wezterm.action.ActivatePaneDirection(dir),
--   })
-- end
table.insert(keys, {
  key = "}",
  mods = "CTRL|SHIFT",
  action = wezterm.action.ActivatePaneDirection "Next",
})
table.insert(keys, {
  key = "{",
  mods = "CTRL|SHIFT",
  action = wezterm.action.ActivatePaneDirection "Prev",
})

table.insert(keys, {
  key = "j",
  mods = "CTRL|ALT",
  action = wezterm.action.ActivateTabRelative(-1),
})
table.insert(keys, {
  key = "k",
  mods = "CTRL|ALT",
  action = wezterm.action.ActivateTabRelative(1),
})
-- table.insert(keys, {
-- 	key = "Left",
-- 	mods = "LEADER",
-- 	action = wezterm.action.ActivateTabRelative(-1),
-- })
-- table.insert(keys, {
-- 	key = "Right",
-- 	mods = "LEADER",
-- 	action = wezterm.action.ActivateTabRelative(1),
-- })

for i = 1, 8 do
  -- ALT + number to activate that tab
  table.insert(keys, {
    key = tostring(i),
    mods = "ALT",
    action = wezterm.action.ActivateTab(i - 1),
  })
end

config.leader = { key = "s", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = keys
config.default_cursor_style = "SteadyBar"

return config
