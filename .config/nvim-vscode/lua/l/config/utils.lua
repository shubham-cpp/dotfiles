local M = {}

---@generic T
---Remove duplicates from table
---@param list T[]
---@return T[]
function M.dedup(list)
  local ret = {}
  local seen = {}
  for _, v in ipairs(list) do
    if not seen[v] then
      table.insert(ret, v)
      seen[v] = true
    end
  end
  return ret
end

---add prefix '<cmd>' and suffix '<cr>'
---@param cmd_name string
---@return string
function M.cmd_str(cmd_name)
  return "<cmd>" .. cmd_name .. "<cr>"
end

return M
