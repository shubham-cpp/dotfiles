local M = {}

local history = {}

local directions = {
  h = "left",
  j = "down",
  k = "up",
  l = "right",
}

local opposite_directions = {
  left = "right",
  right = "left",
  up = "down",
  down = "up",
}

-- Check if a window is a floating window
local function is_floating_window(win_id) return vim.api.nvim_win_get_config(win_id).relative ~= "" end

-- Initialize history for a tab if it doesn't exist
local function ensure_tab_history(tab_id)
  if not history[tab_id] then history[tab_id] = {} end
  return history[tab_id]
end

-- Initialize history for a window if it doesn't exist
local function ensure_window_history(tab_id, win_id)
  local tab_history = ensure_tab_history(tab_id)
  if not tab_history[win_id] then
    tab_history[win_id] = {
      left = nil,
      right = nil,
      up = nil,
      down = nil,
    }
  end
  return tab_history[win_id]
end

-- The main navigation function
function M.navigate(direction_key)
  -- Get current state
  local current_tab_id = vim.api.nvim_get_current_tabpage()
  local current_win_id = vim.api.nvim_get_current_win()

  -- Skip floating windows
  if is_floating_window(current_win_id) then
    vim.cmd("wincmd " .. direction_key)
    return
  end

  -- Get direction and opposite direction
  local direction = directions[direction_key]
  local opposite_direction = opposite_directions[direction]

  -- Store the current window ID before moving
  local old_win_id = current_win_id

  -- Check if we have history for this direction
  local win_history = ensure_window_history(current_tab_id, current_win_id)
  local target_win_id = win_history[direction]

  if target_win_id and vim.api.nvim_win_is_valid(target_win_id) and not is_floating_window(target_win_id) then
    -- We have history, navigate to the target window
    vim.api.nvim_set_current_win(target_win_id)

    -- Update history for the target window to point back to the source
    local target_win_history = ensure_window_history(current_tab_id, target_win_id)
    target_win_history[opposite_direction] = old_win_id
  else
    -- No history or invalid window, use default navigation
    vim.cmd("wincmd " .. direction_key)

    -- Get the new window ID after moving
    local new_win_id = vim.api.nvim_get_current_win()

    -- If we actually moved to a different window, update history
    if new_win_id ~= old_win_id and not is_floating_window(new_win_id) then
      -- Update history for the new window
      local new_win_history = ensure_window_history(current_tab_id, new_win_id)
      new_win_history[opposite_direction] = old_win_id
    end
  end
end

-- Clear history for the current tab
function M.clear_history()
  local current_tab_id = vim.api.nvim_get_current_tabpage()
  history[current_tab_id] = {}
  vim.notify("BetterWinNavigations Via: Navigation history cleared for current tab", vim.log.levels.INFO)
end

-- Setup function to initialize the plugin
-- function M.setup()
--   -- Register the user command to clear history
--   vim.api.nvim_create_user_command("BetterWinNavClearHistory", M.clear_history, {
--     desc = "Clear the window navigation history for the current tab",
--   })
--
--   -- Set up keymappings
--   for _, key in ipairs { "h", "j", "k", "l" } do
--     vim.keymap.set("n", "<C-w>" .. key, function() M.navigate(key) end, { desc = "Smart window navigation: " .. key })
--   end
-- end

return M
