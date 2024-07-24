local M = {}

M.unique_append_table = function(list, values)
  local appendValues = {}
  local l = list or {}
  if not values or next(values) == nil then
    return l
  end
  for _, value in ipairs(values) do
    if not vim.tbl_contains(l, value) then
      table.insert(appendValues, value)
    end
  end
  return vim.list_extend(l, appendValues)
end

return M
