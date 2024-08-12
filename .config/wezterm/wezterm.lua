-- Pull in the wezterm API
local wezterm = require "wezterm"
local session_manager = require "session-manager"

-- This table will hold the configuration.
local config = {}
local keys = {}
local alt_keys = {}
-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end
-- For example, changing the color scheme:
config.color_scheme = "Github Dark (Gogh)"
config.colors = {
  -- background = "#181818",
  tab_bar = {
    -- The color of the strip that goes along the top of the window
    -- (does not apply when fancy tab bar is in use)
    background = "#14131A",

    -- The active tab is the one that has focus in the window
    active_tab = {
      -- The color of the background area for the tab
      bg_color = "#161725",
      -- The color of the text for the tab
      fg_color = "hsl(30, 30%, 75%)",

      -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
      -- label shown for this tab.
      -- The default is "Normal"
      intensity = "Bold",

      -- Specify whether you want "None", "Single" or "Double" underline for
      -- label shown for this tab.
      -- The default is "None"
      underline = "None",

      -- Specify whether you want the text to be italic (true) or not (false)
      -- for this tab.  The default is false.
      italic = false,

      -- Specify whether you want the text to be rendered with strikethrough (true)
      -- or not for this tab.  The default is false.
      strikethrough = false,
    },

    -- Inactive tabs are the tabs that do not have focus
    inactive_tab = {
      bg_color = "hsl(234, 19%, 20%)",
      fg_color = "hsl(234, 55%, 80%)",

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `inactive_tab`.
    },

    -- You can configure some alternate styling when the mouse pointer
    -- moves over inactive tabs
    inactive_tab_hover = {
      bg_color = "#181818",
      fg_color = "#909090",
      italic = true,

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `inactive_tab_hover`.
    },

    -- The new tab button that let you create new tabs
    new_tab = {
      bg_color = "#1b1032",
      fg_color = "#808080",

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `new_tab`.
    },

    -- You can configure some alternate styling when the mouse pointer
    -- moves over the new tab button
    new_tab_hover = {
      bg_color = "#3b3052",
      fg_color = "#909090",
      italic = true,

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `new_tab_hover`.
    },
  },
}
-- Spawn a fish shell in login mode
-- config.default_prog = { "/bin/fish", "-l" }

config.font_size = 11.5
config.font = wezterm.font_with_fallback({
  "FiraCode Nerd Font",
  "JetBrainsMono Nerd Font",
  "FontAwesome",
})
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }
config.switch_to_last_active_tab_when_closing_tab = true

-- default is true, has more "native" look
config.use_fancy_tab_bar = false
config.enable_wayland = false
-- config.front_end = "OpenGL"
config.hide_tab_bar_if_only_one_tab = true
config.window_background_opacity = 0.93
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
config.hyperlink_rules = wezterm.default_hyperlink_rules()
config.mouse_bindings = {
  -- { event = { Up = { streak = 1, button = 'Left' } }, mods = 'NONE', action = wezterm.action.CompleteSelection('PrimarySelection'), },
  { event = { Up = { streak = 1, button = "Left" } }, mods = "CTRL", action = wezterm.action.OpenLinkAtMouseCursor },
}
config.default_cursor_style = "SteadyBar"

wezterm.on("save_session", function(window)
  session_manager.save_state(window)
end)
wezterm.on("load_session", function(window)
  session_manager.load_state(window)
end)
wezterm.on("restore_session", function(window)
  session_manager.restore_state(window)
end)

table.insert(alt_keys, { key = "q", mods = "ALT", action = wezterm.action.CloseCurrentPane({ confirm = false }) })
table.insert(alt_keys, { key = "c", mods = "ALT", action = wezterm.action.CloseCurrentPane({ confirm = false }) })
table.insert(alt_keys, { key = "x", mods = "ALT", action = wezterm.action.CloseCurrentTab({ confirm = false }) })
table.insert(alt_keys, { key = "j", mods = "ALT", action = wezterm.action.ActivatePaneDirection "Down" })
table.insert(alt_keys, { key = "k", mods = "ALT", action = wezterm.action.ActivatePaneDirection "Up" })
table.insert(alt_keys, { key = "l", mods = "ALT", action = wezterm.action.ActivatePaneDirection "Right" })
table.insert(alt_keys, { key = "h", mods = "ALT", action = wezterm.action.ActivatePaneDirection "Left" })
table.insert(alt_keys, { key = "z", mods = "ALT", action = wezterm.action.TogglePaneZoomState })
table.insert(alt_keys, { key = "t", mods = "ALT", action = wezterm.action.SpawnTab "CurrentPaneDomain" })
table.insert(alt_keys, { key = "g", mods = "ALT", action = wezterm.action.ShowTabNavigator })
table.insert(alt_keys, { key = "o", mods = "ALT", action = wezterm.action.ActivateLastTab })
table.insert(alt_keys, { key = ")", mods = "SHIFT|ALT", action = wezterm.action.PaneSelect })
table.insert(
  alt_keys,
  { key = "0", mods = "ALT", action = wezterm.action.PaneSelect({ mode = "SwapWithActiveKeepFocus" }) }
)
table.insert(
  alt_keys,
  { key = "v", mods = "ALT", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) }
)
table.insert(
  alt_keys,
  { key = "s", mods = "ALT", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) }
)
-- table.insert(alt_keys, { key = "j", mods = "ALT|SHIFT", action = wezterm.action.ActivatePaneDirection "Next" })
-- table.insert(alt_keys, { key = "k", mods = "ALT|SHIFT", action = wezterm.action.ActivatePaneDirection "Prev" })

table.insert(alt_keys, { key = "p", mods = "SHIFT|ALT", action = wezterm.action.MoveTabRelative(-1) })
table.insert(alt_keys, { key = "n", mods = "SHIFT|ALT", action = wezterm.action.MoveTabRelative(1) })
table.insert(alt_keys, { key = "p", mods = "ALT", action = wezterm.action.ActivateTabRelative(-1) })
table.insert(alt_keys, { key = "n", mods = "ALT", action = wezterm.action.ActivateTabRelative(1) })
table.insert(alt_keys, { key = ".", mods = "ALT", action = wezterm.action.ActivateCommandPalette })

table.insert(alt_keys, { key = "q", mods = "SHIFT|ALT", action = wezterm.action.QuickSelect })
table.insert(alt_keys, { key = "c", mods = "SHIFT|ALT", action = wezterm.action.ActivateCopyMode })

table.insert(alt_keys, { key = "c", mods = "SHIFT|ALT", action = wezterm.action.ActivateCopyMode })

-- table.insert(alt_keys, { key = "j", mods = "SHIFT|ALT", action = wezterm.action.RotatePanes "Clockwise" })
-- table.insert(alt_keys, { key = "k", mods = "SHIFT|ALT", action = wezterm.action.RotatePanes "CounterClockwise" })

table.insert(alt_keys, { key = "h", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Left", 5 }) })
table.insert(alt_keys, { key = "j", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Down", 5 }) })
table.insert(alt_keys, { key = "k", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Up", 5 }) })
table.insert(alt_keys, { key = "l", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Right", 5 }) })

table.insert(keys, { key = "s", mods = "LEADER|CTRL", action = wezterm.action({ EmitEvent = "save_session" }) })
table.insert(keys, { key = "r", mods = "LEADER|CTRL", action = wezterm.action({ EmitEvent = "restore_session" }) })
table.insert(keys, { key = "l", mods = "LEADER|CTRL", action = wezterm.action({ EmitEvent = "load_session" }) })

-- table.insert(keys, {
--   key = "s",
--   mods = "LEADER|CTRL",
--   action = wezterm.action.SendKey({ key = "s", mods = "CTRL" }),
-- })
table.insert(keys, { key = "j", mods = "LEADER", action = wezterm.action.ActivatePaneDirection "Down" })
table.insert(keys, { key = "k", mods = "LEADER", action = wezterm.action.ActivatePaneDirection "Up" })
table.insert(keys, { key = "l", mods = "LEADER", action = wezterm.action.ActivatePaneDirection "Right" })
table.insert(keys, { key = "h", mods = "LEADER", action = wezterm.action.ActivatePaneDirection "Left" })
table.insert(keys, { key = "z", mods = "LEADER", action = wezterm.action.TogglePaneZoomState })
table.insert(keys, { key = "t", mods = "LEADER", action = wezterm.action.SpawnTab "CurrentPaneDomain" })
table.insert(keys, { key = "g", mods = "LEADER", action = wezterm.action.ShowTabNavigator })
table.insert(keys, { key = "o", mods = "LEADER", action = wezterm.action.ActivateLastTab })
table.insert(keys, { key = "p", mods = "LEADER", action = wezterm.action.PaneSelect })
table.insert(keys, { key = "q", mods = "LEADER", action = wezterm.action.CloseCurrentPane({ confirm = false }) })
table.insert(keys, { key = "x", mods = "LEADER", action = wezterm.action.CloseCurrentTab({ confirm = false }) })

table.insert(
  keys,
  { key = "v", mods = "LEADER", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) }
)
table.insert(
  keys,
  { key = "s", mods = "LEADER", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) }
)
table.insert(keys, {
  key = "P",
  mods = "LEADER|SHIFT",
  action = wezterm.action.PaneSelect({
    mode = "SwapWithActive",
  }),
})

table.insert(
  keys,
  { key = "Enter", mods = "CTRL|SHIFT", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) }
)
table.insert(
  keys,
  { key = "Backspace", mods = "CTRL|SHIFT", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) }
)

table.insert(keys, { key = "c", mods = "CTRL|SHIFT", action = wezterm.action.CopyTo "ClipboardAndPrimarySelection" })
table.insert(keys, { key = "v", mods = "CTRL|SHIFT", action = wezterm.action.PasteFrom "Clipboard" })
table.insert(keys, { key = "t", mods = "SHIFT|CTRL", action = wezterm.action.SpawnTab "CurrentPaneDomain" })
table.insert(keys, { key = "l", mods = "CTRL", action = wezterm.action.DisableDefaultAssignment })
table.insert(keys, { key = "w", mods = "SHIFT|CTRL", action = wezterm.action.CloseCurrentTab({ confirm = false }) })

table.insert(keys, { key = "}", mods = "CTRL|SHIFT", action = wezterm.action.ActivatePaneDirection "Next" })
table.insert(keys, { key = "{", mods = "CTRL|SHIFT", action = wezterm.action.ActivatePaneDirection "Prev" })
table.insert(keys, { key = "{", mods = "SHIFT|ALT", action = wezterm.action.MoveTabRelative(-1) })
table.insert(keys, { key = "}", mods = "SHIFT|ALT", action = wezterm.action.MoveTabRelative(1) })
table.insert(keys, { key = "j", mods = "CTRL|ALT", action = wezterm.action.ActivateTabRelative(-1) })
table.insert(keys, { key = "k", mods = "CTRL|ALT", action = wezterm.action.ActivateTabRelative(1) })
table.insert(keys, { key = ".", mods = "LEADER", action = wezterm.action.ActivateCommandPalette })

local process_icons = {
  ["docker"] = wezterm.nerdfonts.linux_docker,
  ["docker-compose"] = wezterm.nerdfonts.linux_docker,
  ["psql"] = "󱤢",
  ["usql"] = "󱤢",
  ["kuberlr"] = wezterm.nerdfonts.linux_docker,
  ["ssh"] = wezterm.nerdfonts.fa_exchange,
  ["ssh-add"] = wezterm.nerdfonts.fa_exchange,
  ["kubectl"] = wezterm.nerdfonts.linux_docker,
  ["stern"] = wezterm.nerdfonts.linux_docker,
  ["nvim"] = wezterm.nerdfonts.custom_vim,
  ["make"] = wezterm.nerdfonts.seti_makefile,
  ["vim"] = wezterm.nerdfonts.dev_vim,
  ["node"] = wezterm.nerdfonts.mdi_hexagon,
  ["go"] = wezterm.nerdfonts.seti_go,
  ["python3"] = "",
  ["zsh"] = wezterm.nerdfonts.dev_terminal,
  ["bash"] = wezterm.nerdfonts.cod_terminal_bash,
  ["btm"] = wezterm.nerdfonts.mdi_chart_donut_variant,
  ["htop"] = wezterm.nerdfonts.mdi_chart_donut_variant,
  ["cargo"] = wezterm.nerdfonts.dev_rust,
  ["sudo"] = wezterm.nerdfonts.fa_hashtag,
  ["lazydocker"] = wezterm.nerdfonts.linux_docker,
  ["git"] = wezterm.nerdfonts.dev_git,
  ["lua"] = wezterm.nerdfonts.seti_lua,
  ["wget"] = wezterm.nerdfonts.mdi_arrow_down_box,
  ["curl"] = wezterm.nerdfonts.mdi_flattr,
  ["gh"] = wezterm.nerdfonts.dev_github_badge,
  ["ruby"] = wezterm.nerdfonts.cod_ruby,
}
local function get_current_working_dir(tab)
  local pane = tab.active_pane
  local current_dir = pane and pane.current_working_dir or { file_path = "" }
  local HOME_DIR = os.getenv "HOME"

  return current_dir.file_path == HOME_DIR and "~" or string.gsub(current_dir.file_path, "(.*[/\\])(.*)", "%2")
end

local function get_process(tab)
  local pane = tab.active_pane
  if not pane or pane.foreground_process_name == "" then
    return nil
  end

  local process_name = string.gsub(pane.foreground_process_name, "(.*[/\\])(.*)", "%2")
  if string.find(process_name, "kubectl") then
    process_name = "kubectl"
  end

  return process_icons[process_name] or string.format("%s", process_name)
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local has_unseen_output = false
  if not tab.is_active then
    for _, pane in ipairs(tab.panes) do
      if pane.has_unseen_output then
        has_unseen_output = true
        break
      end
    end
  end

  local cwd = wezterm.format({
    { Text = get_current_working_dir(tab) },
  })

  local process = get_process(tab) or ""
  local title = string.format("[%s] %s ", cwd, process) or " [?] "

  if has_unseen_output then
    return {
      { Foreground = { Color = "#28719c" } },
      { Text = title },
    }
  end

  return {
    { Text = title },
  }
end)

for i = 1, 8 do
  -- ALT + number to activate that tab
  table.insert(keys, {
    key = tostring(i),
    mods = "ALT",
    action = wezterm.action.ActivateTab(i - 1),
  })
  table.insert(alt_keys, {
    key = tostring(i),
    mods = "ALT",
    action = wezterm.action.ActivateTab(i - 1),
  })
end

config.leader = { key = "s", mods = "CTRL", timeout_milliseconds = 1000 }
-- config.keys = keys
config.keys = alt_keys

return config
