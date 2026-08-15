-- Lua port of fzy's matching algorithm.
-- Original: https://github.com/jhawthorn/fzy
-- MIT License, Copyright (c) 2014 John Hawthorn.
local M = {}

local pick = require "config.pick"

local SCORE_GAP_LEADING = -0.005
local SCORE_GAP_TRAILING = -0.005
local SCORE_GAP_INNER = -0.01
local SCORE_MATCH_CONSECUTIVE = 1.0
local SCORE_MATCH_SLASH = 0.9
local SCORE_MATCH_WORD = 0.8
local SCORE_MATCH_CAPITAL = 0.7
local SCORE_MATCH_DOT = 0.6

M.SCORE_MAX = math.huge
M.SCORE_MIN = -math.huge
M.MATCH_MAX_LEN = 1024

local function max(a, b) return a > b and a or b end

local function is_lower(ch) return ch:match "%l" ~= nil end

local function is_upper(ch) return ch:match "%u" ~= nil end

local function is_digit(ch) return ch:match "%d" ~= nil end

local function compute_bonus(last_ch, ch)
  if not (is_lower(ch) or is_upper(ch) or is_digit(ch)) then return 0 end
  if last_ch == "/" then return SCORE_MATCH_SLASH end
  if last_ch == "-" or last_ch == "_" or last_ch == " " then return SCORE_MATCH_WORD end
  if last_ch == "." then return SCORE_MATCH_DOT end
  if is_upper(ch) and is_lower(last_ch) then return SCORE_MATCH_CAPITAL end
  return 0
end

local function precompute_bonus(haystack)
  local bonuses, last_ch = {}, "/"
  for i = 1, #haystack do
    local ch = haystack:sub(i, i)
    bonuses[i] = compute_bonus(last_ch, ch)
    last_ch = ch
  end
  return bonuses
end

local function setup_match(needle, haystack)
  local n, m = #needle, #haystack
  return {
    needle_len = n,
    haystack_len = m,
    lower_needle = needle:lower(),
    lower_haystack = haystack:lower(),
    match_bonus = m <= M.MATCH_MAX_LEN and n <= m and precompute_bonus(haystack) or {},
  }
end

local function match_row(match_data, row, last_d, last_m)
  local n, m = match_data.needle_len, match_data.haystack_len
  local lower_needle, lower_haystack = match_data.lower_needle, match_data.lower_haystack
  local match_bonus = match_data.match_bonus
  local curr_d, curr_m = {}, {}
  local prev_score = M.SCORE_MIN
  local gap_score = row == n and SCORE_GAP_TRAILING or SCORE_GAP_INNER
  local prev_d, prev_m = M.SCORE_MIN, M.SCORE_MIN
  local needle_ch = lower_needle:sub(row, row)

  for col = 1, m do
    if needle_ch == lower_haystack:sub(col, col) then
      local score = M.SCORE_MIN
      if row == 1 then
        score = ((col - 1) * SCORE_GAP_LEADING) + match_bonus[col]
      elseif col > 1 then
        score = max(prev_m + match_bonus[col], prev_d + SCORE_MATCH_CONSECUTIVE)
      end

      prev_d = last_d[col] or M.SCORE_MIN
      prev_m = last_m[col] or M.SCORE_MIN
      curr_d[col] = score
      curr_m[col] = max(score, prev_score + gap_score)
    else
      prev_d = last_d[col] or M.SCORE_MIN
      prev_m = last_m[col] or M.SCORE_MIN
      curr_d[col] = M.SCORE_MIN
      curr_m[col] = prev_score + gap_score
    end
    prev_score = curr_m[col]
  end

  return curr_d, curr_m
end

function M.has_match(needle, haystack)
  local from = 1
  local lower_haystack = haystack:lower()
  for i = 1, #needle do
    local found = lower_haystack:find(needle:sub(i, i):lower(), from, true)
    if found == nil then return false end
    from = found + 1
  end
  return true
end

function M.match(needle, haystack)
  if needle == "" then return M.SCORE_MIN end

  local data = setup_match(needle, haystack)
  local n, m = data.needle_len, data.haystack_len
  if m > M.MATCH_MAX_LEN or n > m then return M.SCORE_MIN end
  if n == m then return M.SCORE_MAX end

  local last_d, last_m = {}, {}
  for row = 1, n do
    last_d, last_m = match_row(data, row, last_d, last_m)
  end
  return last_m[m]
end

function M.match_positions(needle, haystack)
  if needle == "" then return M.SCORE_MIN, {} end

  local data = setup_match(needle, haystack)
  local n, m = data.needle_len, data.haystack_len
  if m > M.MATCH_MAX_LEN or n > m then return M.SCORE_MIN, nil end
  if n == m then
    local positions = {}
    for i = 1, n do positions[i] = i - 1 end
    return M.SCORE_MAX, positions
  end

  local d_rows, m_rows = {}, {}
  d_rows[1], m_rows[1] = match_row(data, 1, {}, {})
  for row = 2, n do
    d_rows[row], m_rows[row] = match_row(data, row, d_rows[row - 1], m_rows[row - 1])
  end

  local positions, match_required = {}, false
  local col = m
  for row = n, 1, -1 do
    while col >= 1 do
      local d_score = d_rows[row][col]
      if d_score ~= M.SCORE_MIN and (match_required or d_score == m_rows[row][col]) then
        match_required = row > 1 and col > 1 and m_rows[row][col] == d_rows[row - 1][col - 1] + SCORE_MATCH_CONSECUTIVE
        positions[row] = col - 1
        col = col - 1
        break
      end
      col = col - 1
    end
  end

  return m_rows[n][m], positions
end

local function active_stritems(stritems)
  if _G.MiniPick == nil or not MiniPick.is_picker_active() then return stritems end
  local ok, original = pcall(MiniPick.get_picker_stritems)
  if ok and type(original) == "table" and #original == #stritems then return original end
  return stritems
end

local function active_query(query)
  if _G.MiniPick == nil or not MiniPick.is_picker_active() then return query end
  local ok, original = pcall(MiniPick.get_picker_query)
  if ok and type(original) == "table" then return original end
  return query
end

local function all_indexes(stritems)
  local res = {}
  for i = 1, #stritems do res[i] = i end
  return res
end

function M.match_source(stritems, inds, query)
  local prompt = table.concat(active_query(query))
  if prompt == "" then return all_indexes(stritems) end

  local originals = active_stritems(stritems)
  local is_active = _G.MiniPick ~= nil and MiniPick.is_picker_active()
  local querytick = is_active and MiniPick.get_querytick() or nil

  local function compute()
    local matches = {}
    for _, ind in ipairs(inds) do
      if is_active then
        local stale = not MiniPick.poke_is_picker_active() or MiniPick.get_querytick() ~= querytick
        if stale then return end
      end

      local candidate = originals[ind] or stritems[ind]
      if type(candidate) == "table" then candidate = candidate.path or candidate.text or "" end
      candidate = tostring(candidate or "")
      if M.has_match(prompt, candidate) then
        local score = M.match(prompt, candidate)
        if score ~= M.SCORE_MIN then table.insert(matches, { ind = ind, score = score }) end
      end
    end

    table.sort(matches, function(a, b)
      if a.score == b.score then return a.ind < b.ind end
      return a.score > b.score
    end)

    local match_inds = vim.tbl_map(function(item) return item.ind end, matches)
    if is_active then
      MiniPick.set_picker_match_inds(match_inds)
      return
    end
    return match_inds
  end

  if not is_active then return compute() end
  coroutine.resume(coroutine.create(compute))
end

local function positions_to_ranges(path, positions)
  if positions == nil then return nil end

  local basename, dirname = pick.split_path(path)
  local ranges = {}
  for _, pos in ipairs(positions) do
    local col
    if dirname == "" then
      col = pos
    elseif pos < #dirname then
      col = #basename + 2 + pos
    elseif pos == #dirname then
      col = #basename + 1
    else
      col = pos - #dirname - 1
    end

    if col >= 0 then table.insert(ranges, { col, col + 1 }) end
  end
  return ranges
end

function M.show_filename_first(buf_id, items, query)
  local prompt = table.concat(query or {})
  local ranges = {}
  if prompt ~= "" then
    for i, item in ipairs(items) do
      local path = pick.path_text(item)
      local _, positions = M.match_positions(prompt, path)
      ranges[i] = positions_to_ranges(path, positions)
    end
  end

  pick.show_filename_first(buf_id, items, {}, { match_ranges = ranges })
end

function M.source_opts(source)
  return {
    options = { use_cache = false },
    source = vim.tbl_extend("force", {
      match = M.match_source,
      show = M.show_filename_first,
    }, source or {}),
  }
end

return M
