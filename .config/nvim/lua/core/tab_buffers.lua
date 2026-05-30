local M = {}

local state_var = "TabBuffersState"

local function current_tab()
  return vim.api.nvim_get_current_tabpage()
end

local function tab_bufs(tabpage)
  tabpage = tabpage or current_tab()
  if not vim.t[tabpage].bufs then
    vim.t[tabpage].bufs = {}
  end
  return vim.t[tabpage].bufs
end

local function is_valid(buf)
  return vim.api.nvim_buf_is_valid(buf)
    and vim.fn.buflisted(buf) ~= 0
    and vim.api.nvim_get_option_value("buftype", { buf = buf }) == ""
end

local function is_restorable(buf)
  return is_valid(buf) and vim.api.nvim_buf_get_name(buf) ~= ""
end

local function normalize(bufs)
  local result = {}
  local seen = {}
  for _, buf in ipairs(bufs or {}) do
    if not seen[buf] and is_valid(buf) then
      result[#result + 1] = buf
      seen[buf] = true
    end
  end
  return result
end

local function index_of(bufs, buf)
  for i, item in ipairs(bufs) do
    if item == buf then
      return i
    end
  end
end

local function is_visible_in_tab(buf, tabpage)
  if not vim.api.nvim_tabpage_is_valid(tabpage) then
    return false
  end
  for _, win in ipairs(vim.api.nvim_tabpage_list_wins(tabpage)) do
    if vim.api.nvim_win_get_buf(win) == buf then
      return true
    end
  end
  return false
end

function M.serialize_state()
  if vim.g.SessionLoad == 1 then
    return vim.g[state_var]
  end

  local state = {}
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    local bufs = {}
    for _, buf in ipairs(normalize(tab_bufs(tabpage))) do
      if is_restorable(buf) then
        bufs[#bufs + 1] = vim.api.nvim_buf_get_name(buf)
      end
    end
    state[#state + 1] = bufs
  end
  vim.g[state_var] = vim.json.encode(state)
  return vim.g[state_var]
end

function M.track(buf, tabpage)
  buf = buf or vim.api.nvim_get_current_buf()
  tabpage = tabpage or current_tab()
  if not is_valid(buf) then
    return
  end

  local bufs = normalize(tab_bufs(tabpage))
  if not index_of(bufs, buf) then
    bufs[#bufs + 1] = buf
  end
  vim.t[tabpage].bufs = bufs
  M.serialize_state()
  vim.cmd.redrawtabline()
end

function M.untrack(buf)
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    local bufs = {}
    for _, item in ipairs(tab_bufs(tabpage)) do
      if item ~= buf and is_valid(item) then
        bufs[#bufs + 1] = item
      end
    end
    vim.t[tabpage].bufs = bufs
  end
  M.serialize_state()
  vim.cmd.redrawtabline()
end

function M.clean_tabs()
  local live = {}
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    live[tabpage] = true
  end
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    if live[tabpage] then
      vim.t[tabpage].bufs = normalize(tab_bufs(tabpage))
    end
  end
  M.serialize_state()
end

function M.hydrate_tab(tabpage)
  tabpage = tabpage or current_tab()
  local bufs = normalize(tab_bufs(tabpage))
  local seen = {}
  for _, buf in ipairs(bufs) do
    seen[buf] = true
  end

  for _, win in ipairs(vim.api.nvim_tabpage_list_wins(tabpage)) do
    local buf = vim.api.nvim_win_get_buf(win)
    if not seen[buf] and is_valid(buf) then
      bufs[#bufs + 1] = buf
      seen[buf] = true
    end
  end

  vim.t[tabpage].bufs = bufs
  M.serialize_state()
  vim.cmd.redrawtabline()
end

function M.hydrate_all()
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    M.hydrate_tab(tabpage)
  end
end

function M.hydrate_legacy_session()
  M.hydrate_all()

  local owned = {}
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    for _, buf in ipairs(tab_bufs(tabpage)) do
      owned[buf] = true
    end
  end

  local tabpage = current_tab()
  local bufs = normalize(tab_bufs(tabpage))
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if not owned[buf] and is_restorable(buf) then
      bufs[#bufs + 1] = buf
      owned[buf] = true
    end
  end
  vim.t[tabpage].bufs = bufs
  M.serialize_state()
  vim.cmd.redrawtabline()
end

function M.restore_state()
  local raw = vim.g[state_var]
  if type(raw) ~= "string" or raw == "" then
    M.hydrate_legacy_session()
    return
  end

  local ok, state = pcall(vim.json.decode, raw)
  if not ok or type(state) ~= "table" then
    M.hydrate_legacy_session()
    return
  end

  local name_to_buf = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if is_restorable(buf) then
      name_to_buf[vim.api.nvim_buf_get_name(buf)] = buf
    end
  end

  local tabpages = vim.api.nvim_list_tabpages()
  for index, tabpage in ipairs(tabpages) do
    if type(state[index]) == "table" then
      local bufs = {}
      local seen = {}
      for _, name in ipairs(state[index]) do
        local buf = name_to_buf[name]
        if buf and not seen[buf] then
          bufs[#bufs + 1] = buf
          seen[buf] = true
        end
      end
      vim.t[tabpage].bufs = bufs
    else
      M.hydrate_tab(tabpage)
    end
  end

  M.serialize_state()
  vim.cmd.redrawtabline()
end

function M.get_bufnrs()
  local bufs = normalize(tab_bufs())
  vim.t.bufs = bufs
  return bufs
end

function M.next(count)
  count = count or 1
  local bufs = M.get_bufnrs()
  if #bufs == 0 then
    return
  end
  local cur = vim.api.nvim_get_current_buf()
  local current = index_of(bufs, cur)
  local target = current and ((current - 1 + count) % #bufs) + 1 or 1
  vim.api.nvim_set_current_buf(bufs[target])
end

function M.prev(count)
  M.next(-(count or 1))
end

function M.buf_delete(buf_id, force)
  force = force or false
  if buf_id == nil then
    local cur = vim.api.nvim_get_current_buf()
    for _, buf in ipairs(vim.deepcopy(M.get_bufnrs())) do
      if buf ~= cur and is_valid(buf) then
        vim.cmd((force and "bdelete! " or "confirm bdelete ") .. buf)
      end
    end
    return
  end

  buf_id = buf_id == 0 and vim.api.nvim_get_current_buf() or buf_id
  if not vim.api.nvim_buf_is_valid(buf_id) then
    vim.notify(("Invalid buffer: %s"):format(buf_id), vim.log.levels.ERROR)
    return
  end
  vim.cmd((force and "bdelete! " or "confirm bdelete ") .. buf_id)
end

vim.api.nvim_create_user_command("Bdelete", function(opts)
  local force = opts.bang
  if opts.args == "" then
    M.buf_delete(0, force)
  elseif opts.args == "all" then
    M.buf_delete(nil, force)
  else
    local buf = tonumber(opts.args)
    if not buf then
      vim.notify(("Invalid buffer: %s"):format(opts.args), vim.log.levels.ERROR)
      return
    end
    M.buf_delete(buf, force)
  end
end, {
  nargs = "?",
  bang = true,
  desc = "Delete buffer (tab-scoped). :Bdelete, :Bdelete!, :Bdelete all, :Bdelete <bufnr>",
})

function M.setup()
  local group = vim.api.nvim_create_augroup("tab_buffers", { clear = true })
  local pending_new_tabs = {}
  local schedule_track = function(buf, tabpage)
    if vim.g.SessionLoad == 1 then
      return
    end

    vim.schedule(function()
      if is_visible_in_tab(buf, tabpage) then
        M.track(buf, tabpage)
      end
    end)
  end

  vim.api.nvim_create_autocmd("TabNew", {
    group = group,
    callback = function()
      pending_new_tabs[current_tab()] = true
    end,
  })

  vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter", "TabEnter" }, {
    group = group,
    callback = function(ev)
      local tabpage = current_tab()
      local buf = ev.buf or vim.api.nvim_get_current_buf()
      if not pending_new_tabs[tabpage] then
        M.track(buf, tabpage)
      end
      schedule_track(buf, tabpage)
    end,
  })

  vim.api.nvim_create_autocmd("TabNewEntered", {
    group = group,
    callback = function(ev)
      local tabpage = current_tab()
      pending_new_tabs[tabpage] = nil
      schedule_track(ev.buf or vim.api.nvim_get_current_buf(), tabpage)
    end,
  })

  vim.api.nvim_create_autocmd({ "BufDelete", "BufWipeout", "TermClose" }, {
    group = group,
    callback = function(ev)
      M.untrack(ev.buf)
    end,
  })

  vim.api.nvim_create_autocmd("TabClosed", {
    group = group,
    callback = function()
      M.clean_tabs()
    end,
  })

  vim.api.nvim_create_autocmd("SessionLoadPre", {
    group = group,
    callback = function()
      vim.g[state_var] = nil
    end,
  })

  vim.api.nvim_create_autocmd({ "SessionLoadPost", "VimEnter" }, {
    group = group,
    callback = function()
      vim.schedule(M.restore_state)
    end,
  })

  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = { "PersistenceLoadPost", "PersistenceSavePre" },
    callback = function(ev)
      if ev.match == "PersistenceLoadPost" then
        vim.schedule(M.restore_state)
      else
        M.serialize_state()
      end
    end,
  })

  M.restore_state()
end

return M
