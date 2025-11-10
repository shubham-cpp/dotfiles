-- Pull in the wezterm API
local wezterm = require "wezterm"
local act = wezterm.action
local tabline = wezterm.plugin.require "https://github.com/michaelbrusegard/tabline.wez"
local workspace_switcher = wezterm.plugin.require "https://github.com/MLFlexer/smart_workspace_switcher.wezterm"

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.hyperlink_rules = wezterm.default_hyperlink_rules()
-- For example, changing the color scheme:
config.color_scheme = "iceberg-dark" -- "iceberg-dark" -- "Ashes (dark) (terminal.sexy)"

-- config.colors = { background = "#14131A" }
config.default_prog = { "/bin/fish", "-l" }
config.font = wezterm.font_with_fallback({
  "FiraCode Nerd Font",
  "JetBrainsMono Nerd Font",
  "FontAwesome",
})
config.window_background_opacity = 0.95
config.font_size = 11.5
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }
config.switch_to_last_active_tab_when_closing_tab = true
config.scrollback_lines = 10000
config.check_for_updates = false
config.default_cursor_style = "SteadyBar"
config.enable_wayland = require("os").getenv "XDG_CURRENT_DESKTOP" ~= "Hyprland"

local alt_keys = {}

for i = 1, 8 do
  -- ALT + number to activate that tab
  table.insert(alt_keys, {
    key = tostring(i),
    mods = "ALT",
    action = act.ActivateTab(i - 1),
  })
end

table.insert(alt_keys, { key = "c", mods = "SHIFT|ALT", action = act.CloseCurrentPane({ confirm = false }) })
table.insert(alt_keys, { key = "x", mods = "SHIFT|ALT", action = act.CloseCurrentTab({ confirm = false }) })
table.insert(alt_keys, { key = "j", mods = "ALT", action = act.ActivatePaneDirection "Down" })
table.insert(alt_keys, { key = "k", mods = "ALT", action = act.ActivatePaneDirection "Up" })
table.insert(alt_keys, { key = "l", mods = "ALT", action = act.ActivatePaneDirection "Right" })
table.insert(alt_keys, { key = "h", mods = "ALT", action = act.ActivatePaneDirection "Left" })
table.insert(alt_keys, { key = "z", mods = "ALT", action = act.TogglePaneZoomState })
table.insert(alt_keys, { key = "t", mods = "ALT", action = act.SpawnTab "CurrentPaneDomain" })
table.insert(alt_keys, { key = "g", mods = "ALT", action = act.ShowTabNavigator })
table.insert(alt_keys, { key = "o", mods = "ALT", action = act.ActivateLastTab })
table.insert(alt_keys, { key = ")", mods = "SHIFT|ALT", action = act.PaneSelect })
table.insert(alt_keys, { key = "0", mods = "ALT", action = act.PaneSelect({ mode = "SwapWithActiveKeepFocus" }) })
table.insert(alt_keys, { key = "v", mods = "ALT", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) })
table.insert(alt_keys, { key = "s", mods = "ALT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) })

table.insert(alt_keys, { key = "{", mods = "SHIFT|ALT", action = act.MoveTabRelative(-1) })
table.insert(alt_keys, { key = "}", mods = "SHIFT|ALT", action = act.MoveTabRelative(1) })
table.insert(alt_keys, { key = "[", mods = "ALT", action = act.ActivateTabRelative(-1) })
table.insert(alt_keys, { key = "]", mods = "ALT", action = act.ActivateTabRelative(1) })
table.insert(alt_keys, { key = ".", mods = "ALT", action = act.ActivateCommandPalette })

table.insert(alt_keys, { key = "q", mods = "SHIFT|ALT", action = act.QuickSelect })
table.insert(alt_keys, { key = "c", mods = "ALT", action = act.ActivateCopyMode })

table.insert(alt_keys, { key = "h", mods = "SHIFT|ALT", action = act.AdjustPaneSize({ "Left", 2 }) })
table.insert(alt_keys, { key = "j", mods = "SHIFT|ALT", action = act.AdjustPaneSize({ "Down", 2 }) })
table.insert(alt_keys, { key = "k", mods = "SHIFT|ALT", action = act.AdjustPaneSize({ "Up", 2 }) })
table.insert(alt_keys, { key = "l", mods = "SHIFT|ALT", action = act.AdjustPaneSize({ "Right", 2 }) })

table.insert(alt_keys, { key = "w", mods = "ALT", action = workspace_switcher.switch_workspace() })
table.insert(alt_keys, { key = "w", mods = "SHIFT|ALT", action = workspace_switcher.switch_to_prev_workspace() })

config.keys = alt_keys
config.mouse_bindings = {
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = "NONE",
    action = act.ExtendSelectionToMouseCursor "Cell",
  },
  -- and make CTRL-Click open hyperlinks
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = "CTRL",
    action = "OpenLinkAtMouseCursor",
  },
  -- Disable the 'Down' event of CTRL-Click to avoid weird program behaviors
  {
    event = { Down = { streak = 1, button = "Left" } },
    mods = "CTRL",
    action = act.Nop,
  },

  {
    event = { Down = { streak = 3, button = "Left" } },
    action = act.SelectTextAtMouseCursor "SemanticZone",
    mods = "SHIFT",
  },

  -- Scrolling up while holding CTRL increases the font size
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    mods = "CTRL",
    action = act.IncreaseFontSize,
  },
  -- Scrolling down while holding CTRL decreases the font size
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    mods = "CTRL",
    action = act.DecreaseFontSize,
  },
}

workspace_switcher.apply_to_config(config)

tabline.setup({
  options = {
    theme = config.colors,
    tabs_enabled = true,
    icons_enabled = true,
    section_separators = {
      left = " ",
      right = "",
    },
    component_separators = {
      left = " ",
      right = "",
    },
    tab_separators = {
      left = " ",
      right = "",
    },
  },
  sections = {
    tabline_a = {},
    tabline_b = {},
    tabline_c = {},
    tab_active = {
      "index",
      { "process", icons_only = true, padding = 0 },
      {
        "cwd",
        max_length = 28,
        padding = { left = 0, right = 1 },
      },
      { "zoomed",  padding = 0 },
    },
    tab_inactive = {
      "index",
      wezterm.nerdfonts.cod_folder,
      { "cwd", max_length = 32, padding = { left = 1, right = 1 } },
    },
    tabline_x = { "workspace" },
    tabline_y = { "domain" },
    tabline_z = {},
  },
  extensions = {
    "smart_workspace_switcher",
  },
})
tabline.apply_to_config(config)
config.window_decorations = "TITLE|RESIZE"

-- and finally, return the configuration to wezterm
return config
