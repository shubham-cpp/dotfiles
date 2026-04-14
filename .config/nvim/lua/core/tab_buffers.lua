local M = {}

-- tabpage_id -> { [bufnr] = true }
local tab_bufs = {}

local function tab_id()
  return tostring(vim.api.nvim_get_current_tabpage())
end

function M.track(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  if vim.fn.buflisted(buf) == 0 then
    return
  end
  local bt = vim.api.nvim_get_option_value("buftype", { buf = buf })
  if bt == "quickfix" then
    return
  end
  local id = tab_id()
  tab_bufs[id] = tab_bufs[id] or {}
  tab_bufs[id][buf] = true
end

function M.untrack(buf)
  for _, set in pairs(tab_bufs) do
    set[buf] = nil
  end
end

function M.clean_tab(tp)
  tab_bufs[tostring(tp)] = nil
end

function M.get_bufnrs()
  local id = tab_id()
  local set = tab_bufs[id] or {}
  local result = {}
  for buf in pairs(set) do
    if vim.fn.buflisted(buf) ~= 0 then
      result[#result + 1] = buf
    end
  end
  table.sort(result)
  return result
end

function M.next(count)
  count = count or 1
  local bufs = M.get_bufnrs()
  if #bufs == 0 then
    return
  end
  local cur = vim.api.nvim_get_current_buf()
  for i, b in ipairs(bufs) do
    if b == cur then
      local target = ((i - 1 + count) % #bufs) + 1
      vim.api.nvim_set_current_buf(bufs[target])
      return
    end
  end
  vim.api.nvim_set_current_buf(bufs[1])
end

function M.prev(count)
  M.next(-(count or 1))
end

function M.buf_delete(buf_id, force)
  force = force or false
  if buf_id == nil then
    local bufs = M.get_bufnrs()
    local cur = vim.api.nvim_get_current_buf()
    for _, b in ipairs(bufs) do
      if b ~= cur then
        pcall(vim.api.nvim_buf_delete, b, { force = force })
      end
    end
  else
    buf_id = buf_id == 0 and vim.api.nvim_get_current_buf() or buf_id
    vim.api.nvim_buf_delete(buf_id, { force = force })
  end
end

vim.api.nvim_create_user_command("Bdelete", function(opts)
  local force = opts.bang
  if opts.args == "" then
    M.buf_delete(0, force)
  elseif opts.args == "all" then
    M.buf_delete(nil, force)
  else
    M.buf_delete(tonumber(opts.args), force)
  end
end, {
  nargs = "?",
  bang = true,
  desc = "Delete buffer (tab-scoped). :Bdelete, :Bdelete!, :Bdelete all, :Bdelete <bufnr>",
})

function M.setup()
  local group = vim.api.nvim_create_augroup("tab_buffers", { clear = true })
  vim.api.nvim_create_autocmd("BufEnter", {
    group = group,
    callback = function()
      M.track()
    end,
  })
  vim.api.nvim_create_autocmd("BufDelete", {
    group = group,
    callback = function(ev)
      M.untrack(ev.buf)
    end,
  })
  vim.api.nvim_create_autocmd("TabClosed", {
    group = group,
    callback = function(ev)
      M.clean_tab(ev.tab)
    end,
  })
  M.track()
end

return M
