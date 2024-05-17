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
  -- "FiraCode Nerd Font",
  "JetBrainsMono Nerd Font",
  -- "Symbols Nerd Font Mono",
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
  local current_dir = tab.active_pane and tab.active_pane.current_working_dir or { file_path = "" }
  local HOME_DIR = os.getenv "HOME"

  return current_dir.file_path == HOME_DIR and "~" or string.gsub(current_dir.file_path, "(.*[/\\])(.*)", "%2")
end

local function get_process(tab)
  if not tab.active_pane or tab.active_pane.foreground_process_name == "" then
    return nil
  end

  local process_name = string.gsub(tab.active_pane.foreground_process_name, "(.*[/\\])(.*)", "%2")
  if string.find(process_name, "kubectl") then
    process_name = "kubectl"
  end

  return process_icons[process_name] or string.format("[%s]", process_name)
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

  local process = get_process(tab)
  local title = process and string.format("%s (%s) ", process, cwd) or " [?] "

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
end

config.leader = { key = "s", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = keys

return config
