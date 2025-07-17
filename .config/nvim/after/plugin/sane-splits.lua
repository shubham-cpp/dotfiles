-- Smart Window Navigation Plugin
-- Uses window-scoped and tab-scoped variables to track navigation patterns

-- Direction mappings
local directions = {
  h = "left",
  j = "down",
  k = "up",
  l = "right",
}

-- Opposite directions for tracking
local opposite_directions = {
  left = "right",
  right = "left",
  up = "down",
  down = "up",
}

-- Get window-scoped variable safely
local function get_win_var(winid, var_name)
  local ok, value = pcall(vim.api.nvim_win_get_var, winid, var_name)
  return ok and value or nil
end

-- Set window-scoped variable safely
local function set_win_var(winid, var_name, value)
  pcall(vim.api.nvim_win_set_var, winid, var_name, value)
end

-- Get tab-scoped variable safely
local function get_tab_var(tabid, var_name)
  local ok, value = pcall(vim.api.nvim_tabpage_get_var, tabid, var_name)
  return ok and value or nil
end

-- Set tab-scoped variable safely
local function set_tab_var(tabid, var_name, value)
  pcall(vim.api.nvim_tabpage_set_var, tabid, var_name, value)
end

-- Initialize windows_info for a window
local function init_window_info(winid)
  local windows_info = get_win_var(winid, "windows_info") or {}
  if not windows_info[winid] then
    windows_info[winid] = {
      left = nil,
      right = nil,
      up = nil,
      down = nil,
    }
    set_win_var(winid, "windows_info", windows_info)
  end
  return windows_info
end

-- Get current window position relative to target window
local function get_relative_direction(current_win, target_win)
  local current_pos = vim.api.nvim_win_get_position(current_win)
  local target_pos = vim.api.nvim_win_get_position(target_win)

  local row_diff = target_pos[1] - current_pos[1]
  local col_diff = target_pos[2] - current_pos[2]

  -- Determine primary direction based on larger difference
  if math.abs(col_diff) > math.abs(row_diff) then
    return col_diff > 0 and "right" or "left"
  else
    return row_diff > 0 and "down" or "up"
  end
end

-- Handle WinLeave event
local function on_win_leave()
  local current_win = vim.api.nvim_get_current_win()
  local current_tab = vim.api.nvim_get_current_tabpage()

  -- Update last_window_focused
  set_tab_var(current_tab, "last_window_focused", current_win)
end

-- Handle WinEnter event
local function on_win_enter()
  local current_win = vim.api.nvim_get_current_win()
  local current_tab = vim.api.nvim_get_current_tabpage()

  -- Get last focused window
  local last_window_focused = get_tab_var(current_tab, "last_window_focused")

  if last_window_focused and vim.api.nvim_win_is_valid(last_window_focused) and last_window_focused ~= current_win then
    -- Initialize windows_info for current window
    local windows_info = init_window_info(current_win)

    -- Determine direction from last window to current window
    local direction_from_last = get_relative_direction(last_window_focused, current_win)

    -- Update the opposite direction in current window's info
    local opposite_dir = opposite_directions[direction_from_last]
    windows_info[current_win][opposite_dir] = last_window_focused

    -- Save updated windows_info
    set_win_var(current_win, "windows_info", windows_info)
  end
end

-- Smart window movement
function _G.smart_wincmd(direction_key)
  local current_win = vim.api.nvim_get_current_win()
  local direction = directions[direction_key]

  if not direction then
    vim.cmd("wincmd " .. direction_key)
    return
  end

  -- Get windows_info for current window
  local windows_info = get_win_var(current_win, "windows_info") or {}
  local current_win_info = windows_info[current_win]

  if current_win_info and current_win_info[direction] then
    local target_win_id = current_win_info[direction]

    -- Verify target window is still valid and in the expected direction
    if vim.api.nvim_win_is_valid(target_win_id) then
      local target_pos = vim.api.nvim_win_get_position(target_win_id)
      local current_pos = vim.api.nvim_win_get_position(current_win)

      local is_valid_direction = false
      if direction == "left" and target_pos[2] < current_pos[2] then
        is_valid_direction = true
      elseif direction == "right" and target_pos[2] > current_pos[2] then
        is_valid_direction = true
      elseif direction == "up" and target_pos[1] < current_pos[1] then
        is_valid_direction = true
      elseif direction == "down" and target_pos[1] > current_pos[1] then
        is_valid_direction = true
      end

      if is_valid_direction then
        vim.api.nvim_set_current_win(target_win_id)
        return
      end
    end
  end

  -- Fallback to normal window command
  vim.cmd("wincmd " .. direction_key)
end

-- Setup autocommands
local function setup_tracking()
  local group = vim.api.nvim_create_augroup("SmartWindowNav", { clear = true })

  vim.api.nvim_create_autocmd("WinLeave", {
    group = group,
    callback = on_win_leave,
  })

  vim.api.nvim_create_autocmd("WinEnter", {
    group = group,
    callback = on_win_enter,
  })
end

-- Setup tracking
setup_tracking()

-- Debug function to show tracked windows
local function debug_show_tracked()
  local current_win = vim.api.nvim_get_current_win()
  local current_tab = vim.api.nvim_get_current_tabpage()

  local windows_info = get_win_var(current_win, "windows_info") or {}
  local last_window_focused = get_tab_var(current_tab, "last_window_focused")

  print("=== Smart Window Navigation Debug ===")
  print(string.format("Current window: %d", current_win))
  print(string.format("Last window focused: %s", last_window_focused or "none"))

  print("\nWindows info for current window:")
  local current_win_info = windows_info[current_win]
  if current_win_info then
    for dir, win_id in pairs(current_win_info) do
      if win_id then
        print(string.format("  %s: %d", dir, win_id))
      end
    end
  else
    print("  No windows info found")
  end

  print("\nAll windows info:")
  for win_id, info in pairs(windows_info) do
    print(string.format("  Window %d:", win_id))
    for dir, target_id in pairs(info) do
      if target_id then
        print(string.format("    %s: %d", dir, target_id))
      end
    end
  end
end

-- Clear tracking data (useful for testing)
local function clear_tracking()
  local current_win = vim.api.nvim_get_current_win()
  local current_tab = vim.api.nvim_get_current_tabpage()

  set_win_var(current_win, "windows_info", {})
  set_tab_var(current_tab, "last_window_focused", nil)

  print("Tracking data cleared")
end

for _, dir in ipairs({ "h", "j", "k", "l" }) do
  vim.keymap.set("n", "<C-w><C-" .. dir .. ">", function()
    smart_wincmd(dir)
  end, { desc = "Smart split " .. dir })
  vim.keymap.set("n", "<C-w>" .. dir, function()
    return "<C-w><C-" .. dir .. ">"
  end, { desc = "Smart split " .. dir, remap = false, noremap = true, expr = true })
end
