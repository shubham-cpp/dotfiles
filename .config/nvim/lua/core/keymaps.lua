-- Better window navigation (inlined module)
local nav_history = {}

local nav_directions = {
  h = "left",
  j = "down",
  k = "up",
  l = "right",
}

local nav_opposite = {
  left = "right",
  right = "left",
  up = "down",
  down = "up",
}

local function is_floating(win_id)
  return vim.api.nvim_win_get_config(win_id).relative ~= ""
end

local function ensure_window_history(tab_id, win_id)
  if not nav_history[tab_id] then
    nav_history[tab_id] = {}
  end
  if not nav_history[tab_id][win_id] then
    nav_history[tab_id][win_id] = { left = nil, right = nil, up = nil, down = nil }
  end
  return nav_history[tab_id][win_id]
end

local function smart_navigate(direction_key)
  local current_tab = vim.api.nvim_get_current_tabpage()
  local current_win = vim.api.nvim_get_current_win()

  if is_floating(current_win) then
    vim.cmd("wincmd " .. direction_key)
    return
  end

  local direction = nav_directions[direction_key]
  local opposite = nav_opposite[direction]
  local old_win = current_win

  local win_hist = ensure_window_history(current_tab, current_win)
  local target_win = win_hist[direction]

  if target_win and vim.api.nvim_win_is_valid(target_win) and not is_floating(target_win) then
    vim.api.nvim_set_current_win(target_win)
    local target_hist = ensure_window_history(current_tab, target_win)
    target_hist[opposite] = old_win
  else
    vim.cmd("wincmd " .. direction_key)
    local new_win = vim.api.nvim_get_current_win()
    if new_win ~= old_win and not is_floating(new_win) then
      local new_hist = ensure_window_history(current_tab, new_win)
      new_hist[opposite] = old_win
    end
  end
end

--- Show floating diagnostic on jump
---@param count -1|1 1 Jumps forward, -1 jumps backwards
---@param is_error boolean
local function diagnostic_jump_with_popup(count, is_error)
  local severity = is_error == true and vim.diagnostic.severity.ERROR or nil
  vim.diagnostic.jump({ count = count or 1, severity = severity })
  vim.schedule(function()
    vim.diagnostic.open_float()
  end)
end

-- Movement
vim.keymap.set("", "0", "^", { silent = false })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true })
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true })

-- Saner search
vim.keymap.set({ "n", "x", "o" }, "n", "'Nn'[v:searchforward]", { expr = true })
vim.keymap.set({ "n", "x", "o" }, "N", "'nN'[v:searchforward]", { expr = true })

-- Visual indent (keep selection)
vim.keymap.set("x", "<", "<gv")
vim.keymap.set("x", ">", ">gv")
vim.keymap.set("v", "<Tab>", ">gv")
vim.keymap.set("v", "<S-Tab>", "<gv")

-- Visual paste without copy
vim.keymap.set("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })

-- Black hole register for change/delete
vim.keymap.set("n", "dl", '"_dl')
vim.keymap.set("v", "D", '"_D')
vim.keymap.set({ "n", "v" }, "c", '"_c')
vim.keymap.set("n", "C", '"_C')

-- Save
vim.keymap.set("n", ",w", "<cmd>w!<cr>", { desc = "Save file" })
vim.keymap.set("n", ",W", "<cmd>noautocmd w!<cr>", { desc = "Save file (noautocmd)" })

-- Edit in same dir
vim.keymap.set(
  "n",
  "<localleader>e",
  ':e <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir" }
)
vim.keymap.set(
  "n",
  "<localleader>t",
  ':tabe <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir (Tab)" }
)
vim.keymap.set(
  "n",
  "<localleader>v",
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir (Split)" }
)

-- Buffer navigation
vim.keymap.set("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Prev buffer" })
vim.keymap.set("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next buffer" })

-- Tab-scoped buffer cycling
vim.keymap.set("n", "]b", function()
  require("core.tab_buffers").next(1)
end, { desc = "Next buffer (tab)" })
vim.keymap.set("n", "[b", function()
  require("core.tab_buffers").prev(1)
end, { desc = "Prev buffer (tab)" })

-- Tab-scoped buffer delete
vim.keymap.set("n", "<leader>bd", function()
  require("core.tab_buffers").buf_delete(0)
end, { desc = "Delete buffer" })
vim.keymap.set("n", "<leader>bD", function()
  require("core.tab_buffers").buf_delete(0, true)
end, { desc = "Delete buffer (force)" })
vim.keymap.set("n", "<leader>bo", function()
  require("core.tab_buffers").buf_delete(nil)
end, { desc = "Delete other buffers (tab)" })

-- Window resize
vim.keymap.set("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase height" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease height" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease width" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase width" })

-- Quickfix
vim.keymap.set("n", "]q", "<cmd>cnext<cr>", { desc = "Next quickfix" })
vim.keymap.set("n", "[q", "<cmd>cprev<cr>", { desc = "Prev quickfix" })
-- Next/prev error diagnostic
vim.keymap.set("n", "]e", function()
  diagnostic_jump_with_popup(1, true)
end, { desc = "Next error" })
vim.keymap.set("n", "[e", function()
  diagnostic_jump_with_popup(-1, true)
end, { desc = "Previous error" })
vim.keymap.set("n", "]d", function()
  diagnostic_jump_with_popup(1, false)
end, { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", function()
  diagnostic_jump_with_popup(-1, false)
end, { desc = "Previous Diagnostic" })

-- Next/prev tab
vim.keymap.set("n", "]t", "<cmd>tabnext<cr>", { desc = "Next tab" })
vim.keymap.set("n", "[t", "<cmd>tabprevious<cr>", { desc = "Previous tab" })

-- Tab navigation
for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto Tab " .. i })
end

-- Plugin manager
vim.keymap.set("n", "<leader>L", function()
  vim.cmd("lua print(vim.inspect(vim.pack.get()))")
end, { desc = "Pack info" })

-- Smart window navigation: <C-w>{h,j,k,l} and <C-{h,j,k,l}>
for _, key in ipairs({ "h", "j", "k", "l" }) do
  vim.keymap.set("n", "<C-w>" .. key, function()
    smart_navigate(key)
  end, { desc = "Smart window nav: " .. key })
  vim.keymap.set("n", "<C-" .. key .. ">", function()
    smart_navigate(key)
  end, { desc = "Smart window nav: " .. key })
end

-- Diagnostic float (works with both LSP and nvim-lint diagnostics)
vim.keymap.set("n", "gl", vim.diagnostic.open_float, { desc = "Line Diagnostics" })

vim.keymap.set("n", "<Esc>", function()
  if vim.v.hlsearch then
    vim.cmd("nohlsearch")
    return "<esc>"
  end
  return "<esc>"
end, { expr = true })
