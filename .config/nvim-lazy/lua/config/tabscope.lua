local M = {}

function M.setup()
  require("scope").setup({})
end

function M.suspend()
  pcall(vim.api.nvim_clear_autocmds, { group = "ScopeAU" })
end

function M.save_state()
  if vim.fn.exists(":ScopeSaveState") == 2 then
    vim.cmd("silent! ScopeSaveState")
  end
end

function M.load_state()
  if vim.fn.exists(":ScopeLoadState") == 2 then
    vim.cmd("silent! ScopeLoadState")
  end
end

function M.refresh_bufferline()
  vim.schedule(function()
    if _G.nvim_bufferline then
      pcall(_G.nvim_bufferline)
    end
    pcall(vim.cmd, "redrawtabline")
  end)
end

local function listed_buffers()
  return vim
    .iter(vim.api.nvim_list_bufs())
    :filter(function(buf)
      return vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].buflisted
    end)
    :totable()
end

local function buffer_exists_in_other_tabs(scope, buf)
  local current_tab = vim.api.nvim_get_current_tabpage()

  for tab, buffers in pairs(scope.cache or {}) do
    if tab ~= current_tab and vim.tbl_contains(buffers, buf) then
      return true
    end
  end

  return false
end

local function switch_away_from(buf, buffers)
  if vim.api.nvim_get_current_buf() ~= buf then
    return
  end

  for _, candidate in ipairs(buffers) do
    if candidate ~= buf and vim.api.nvim_buf_is_valid(candidate) then
      vim.api.nvim_set_current_buf(candidate)
      return
    end
  end

  vim.cmd("enew")
end

---@param buf? integer
function M.close_buffer(buf)
  buf = buf or vim.api.nvim_get_current_buf()

  local ok, scope = pcall(require, "scope.core")
  if ok and scope.revalidate then
    scope.revalidate()
  end

  if ok and buffer_exists_in_other_tabs(scope, buf) then
    switch_away_from(buf, listed_buffers())
    if vim.api.nvim_buf_is_valid(buf) then
      vim.bo[buf].buflisted = false
    end
  else
    Snacks.bufdelete(buf)
  end

  if ok and scope.revalidate then
    scope.revalidate()
  end

  M.refresh_bufferline()
end

function M.close_selected_picker_buffers(picker)
  picker.preview:reset()

  local non_buf_delete_requested = false
  for _, item in ipairs(picker:selected({ fallback = true })) do
    if item.buf then
      M.close_buffer(item.buf)
    else
      non_buf_delete_requested = true
    end
  end

  if non_buf_delete_requested then
    Snacks.notify.warn("Only open buffers can be deleted", { title = "Snacks Picker" })
  end

  picker:refresh()
end

function M.resume()
  M.setup()
  M.load_state()
  M.refresh_bufferline()
end

return M
