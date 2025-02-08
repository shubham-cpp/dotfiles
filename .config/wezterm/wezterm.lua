-- -- Pull in the wezterm API
-- local wezterm = require "wezterm"
-- local session_manager = require "session-manager"
-- local bar = wezterm.plugin.require "https://github.com/adriankarlen/bar.wezterm"
--
-- -- This table will hold the configuration.
-- local config = {}
-- local keys = {}
-- local alt_keys = {}
-- -- In newer versions of wezterm, use the config_builder which will
-- -- help provide clearer error messages
-- if wezterm.config_builder then
--   config = wezterm.config_builder()
-- end
-- -- For example, changing the color scheme:
-- config.color_scheme = "Ayu Mirage" -- Everforest Dark Hard (Gogh)
-- config.colors = { background = "#14131A" }
-- config.window_background_opacity = 0.95
-- config.scrollback_lines = 13500
--
-- config.font_size = 12.5
-- config.font = wezterm.font_with_fallback({
--   -- "Iosevka SP Extended",
--   "FiraCode Nerd Font",
--   -- "RecMonoDuotone Nerd Font",
--   -- "CaskaydiaCove Nerd Font",
--   "JetBrainsMono Nerd Font",
--   "FontAwesome",
-- })
--
-- config.hyperlink_rules = wezterm.default_hyperlink_rules()
--
-- for i = 1, 8 do
--   -- ALT + number to activate that tab
--   table.insert(keys, {
--     key = tostring(i),
--     mods = "ALT",
--     action = wezterm.action.ActivateTab(i - 1),
--   })
--   table.insert(alt_keys, {
--     key = tostring(i),
--     mods = "ALT",
--     action = wezterm.action.ActivateTab(i - 1),
--   })
-- end
--
-- config.keys = alt_keys
--
-- -- bar.apply_to_config(config, {
-- --   position = "top",
-- --   modules = {
-- --     workspace = { enabled = false },
-- --     clock = { enabled = false },
-- --     hostname = { enabled = false },
-- --     username = { enabled = false },
-- --   },
-- -- })
-- -- tab bar
-- config.hide_tab_bar_if_only_one_tab = false
-- config.tab_bar_at_bottom = false
-- config.use_fancy_tab_bar = false
-- config.tab_and_split_indices_are_zero_based = true
--
-- local function get_current_working_dir(tab)
--   local pane = tab.active_pane
--   local current_dir = pane and pane.current_working_dir or { file_path = "" }
--   local HOME_DIR = os.getenv "HOME"
--
--   return current_dir.file_path == HOME_DIR and "~" or string.gsub(current_dir.file_path, "(.*[/\\])(.*)", "%2")
-- end
--
-- wezterm.on("update-right-status", function(window, _)
--   local SOLID_LEFT_ARROW = ""
--   local ARROW_FOREGROUND = { Foreground = { Color = "#c6a0f6" } }
--   local prefix = ""
--
--   if window:leader_is_active() then
--     prefix = " " .. utf8.char(0x1f30a) -- ocean wave
--     SOLID_LEFT_ARROW = utf8.char(0xe0b2)
--   end
--
--   if window:active_tab():tab_id() ~= 0 then
--     ARROW_FOREGROUND = { Foreground = { Color = "#1e2030" } }
--   end -- arrow color based on if tab is first pane
--
--   window:set_left_status(wezterm.format({
--     { Background = { Color = "#b7bdf8" } },
--     { Text = prefix },
--     { Text = get_current_working_dir(window:active_tab()) },
--     ARROW_FOREGROUND,
--     { Text = SOLID_LEFT_ARROW },
--   }))
-- end)
--
-- return config
-- WezTerm Keybindings Documentation by dragonlobster
-- ===================================================
-- Leader Key:
-- The leader key is set to ALT + q, with a timeout of 2000 milliseconds (2 seconds).
-- To execute any keybinding, press the leader key (ALT + q) first, then the corresponding key.

-- Keybindings:
-- 1. Tab Management:
--    - LEADER + c: Create a new tab in the current pane's domain.
--    - LEADER + x: Close the current pane (with confirmation).
--    - LEADER + b: Switch to the previous tab.
--    - LEADER + n: Switch to the next tab.
--    - LEADER + <number>: Switch to a specific tab (0–9).

-- 2. Pane Splitting:
--    - LEADER + |: Split the current pane horizontally into two panes.
--    - LEADER + -: Split the current pane vertically into two panes.

-- 3. Pane Navigation:
--    - LEADER + h: Move to the pane on the left.
--    - LEADER + j: Move to the pane below.
--    - LEADER + k: Move to the pane above.
--    - LEADER + l: Move to the pane on the right.

-- 4. Pane Resizing:
--    - LEADER + LeftArrow: Increase the pane size to the left by 5 units.
--    - LEADER + RightArrow: Increase the pane size to the right by 5 units.
--    - LEADER + DownArrow: Increase the pane size downward by 5 units.
--    - LEADER + UpArrow: Increase the pane size upward by 5 units.

-- 5. Status Line:
--    - The status line indicates when the leader key is active, displaying an ocean wave emoji (🌊).

-- Miscellaneous Configurations:
-- - Tabs are shown even if there's only one tab.
-- - The tab bar is located at the bottom of the terminal window.
-- - Tab and split indices are zero-based.

-- Pull in the wezterm API
local wezterm = require "wezterm"

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- For example, changing the color scheme:
config.color_scheme = "Ashes (dark) (terminal.sexy)"
-- config.colors = { background = "#14131A" }

config.font = wezterm.font_with_fallback({
  "FiraCode Nerd Font",
  "JetBrainsMono Nerd Font",
  "FontAwesome",
})
config.window_background_opacity = 0.95
config.font_size = 13
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }
config.switch_to_last_active_tab_when_closing_tab = true
config.scrollback_lines = 10000
config.check_for_updates = false
config.default_cursor_style = "SteadyBar"

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

local alt_keys = {}
for i = 1, 8 do
  -- ALT + number to activate that tab
  table.insert(alt_keys, {
    key = tostring(i),
    mods = "ALT",
    action = wezterm.action.ActivateTab(i - 1),
  })
end

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

table.insert(alt_keys, { key = "p", mods = "SHIFT|ALT", action = wezterm.action.MoveTabRelative(-1) })
table.insert(alt_keys, { key = "n", mods = "SHIFT|ALT", action = wezterm.action.MoveTabRelative(1) })
table.insert(alt_keys, { key = "p", mods = "ALT", action = wezterm.action.ActivateTabRelative(-1) })
table.insert(alt_keys, { key = "n", mods = "ALT", action = wezterm.action.ActivateTabRelative(1) })
table.insert(alt_keys, { key = ".", mods = "ALT", action = wezterm.action.ActivateCommandPalette })

table.insert(alt_keys, { key = "q", mods = "SHIFT|ALT", action = wezterm.action.QuickSelect })
table.insert(alt_keys, { key = "c", mods = "SHIFT|ALT", action = wezterm.action.ActivateCopyMode })

table.insert(alt_keys, { key = "h", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Left", 2 }) })
table.insert(alt_keys, { key = "j", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Down", 2 }) })
table.insert(alt_keys, { key = "k", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Up", 2 }) })
table.insert(alt_keys, { key = "l", mods = "SHIFT|ALT", action = wezterm.action.AdjustPaneSize({ "Right", 2 }) })

config.keys = alt_keys

-- tab bar
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.tab_and_split_indices_are_zero_based = true

local process_icons = {
  ["bash"] = wezterm.nerdfonts.cod_terminal_bash,
  ["btm"] = wezterm.nerdfonts.mdi_chart_donut_variant,
  ["cargo"] = wezterm.nerdfonts.dev_rust,
  ["curl"] = wezterm.nerdfonts.mdi_flattr,
  ["docker"] = wezterm.nerdfonts.linux_docker,
  ["docker-compose"] = wezterm.nerdfonts.linux_docker,
  ["gh"] = wezterm.nerdfonts.dev_github_badge,
  ["git"] = wezterm.nerdfonts.fa_git,
  ["go"] = wezterm.nerdfonts.seti_go,
  ["htop"] = wezterm.nerdfonts.mdi_chart_donut_variant,
  ["kubectl"] = wezterm.nerdfonts.linux_docker,
  ["kuberlr"] = wezterm.nerdfonts.linux_docker,
  ["lazydocker"] = wezterm.nerdfonts.linux_docker,
  ["lazygit"] = wezterm.nerdfonts.oct_git_compare,
  ["lua"] = wezterm.nerdfonts.seti_lua,
  ["make"] = wezterm.nerdfonts.seti_makefile,
  ["node"] = wezterm.nerdfonts.mdi_hexagon,
  ["nvim"] = wezterm.nerdfonts.custom_vim,
  ["psql"] = "󱤢",
  ["ruby"] = wezterm.nerdfonts.cod_ruby,
  ["stern"] = wezterm.nerdfonts.linux_docker,
  ["sudo"] = wezterm.nerdfonts.fa_hashtag,
  ["usql"] = "󱤢",
  ["vim"] = wezterm.nerdfonts.dev_vim,
  ["wget"] = wezterm.nerdfonts.mdi_arrow_down_box,
  ["zsh"] = wezterm.nerdfonts.dev_terminal,
}

-- Return the Tab's current working directory
local function get_cwd(tab)
  return tab.active_pane.current_working_dir.file_path or ""
end
local function remove_abs_path(path)
  return path:gsub("(.*[/\\])(.*)", "%2")
end
-- Return the pretty path of the tab's current working directory
local function get_display_cwd(tab)
  local current_dir = get_cwd(tab)
  local HOME_DIR = string.format("file://%s", os.getenv "HOME")
  return current_dir == HOME_DIR and "~/" or remove_abs_path(current_dir)
end
-- Return the concise name or icon of the running process for display
local function get_process(tab)
  if not tab.active_pane or tab.active_pane.foreground_process_name == "" then
    return "[?]"
  end

  local process_name = remove_abs_path(tab.active_pane.foreground_process_name)
  if process_name:find "kubectl" then
    process_name = "kubectl"
  end

  return process_icons[process_name] or string.format("[%s]", process_name)
end
-- Pretty format the tab title
local function format_title(tab)
  local cwd = get_display_cwd(tab)
  local process = get_process(tab)

  local active_title = tab.active_pane.title
  if active_title:find "- NVIM" then
    active_title = active_title:gsub("^([^ ]+) .*", "%1")
  end
  return string.format(" %d:%s %s ", tab.tab_index + 1, process, cwd)
end
-- Returns manually set title (from `tab:set_title()` or `wezterm cli set-tab-title`) or creates a new one
local function get_tab_title(tab)
  local title = tab.tab_title
  if title and #title > 0 then
    return title
  end
  return format_title(tab)
end
wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local title = get_tab_title(tab)

  if tab.is_active then
    return {
      { Attribute = { Intensity = "Bold" } },
      { Text = title },
    }
  end
  return title
end)

-- and finally, return the configuration to wezterm
return config
