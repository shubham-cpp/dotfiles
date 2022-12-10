local M = {}

--- Run a shell command and return output
---@param cmd string cmd to run
---@return string output of `cmd`
function M.shell_run(cmd)
  local result = 0
  local output = io.popen(cmd)
  if not output then
    return 'pactl return error.'
  end
  result = output:read '*a'
  output:close()
  return result
end

--- Function to return output of shell command or nil
---@param cmd string cmd to run
---@param raw boolean? whether or not to remove spaces and empty lines
---@return string?
function M.capture(cmd, raw)
  local handle = assert(io.popen(cmd, 'r'))
  local output = assert(handle:read '*a')

  handle:close()

  if raw then
    return output
  end

  output = string.gsub(string.gsub(string.gsub(output, '^%s+', ''), '%s+$', ''), '[\n\r]+', ' ')

  return output
end

function M.tableToString(t)
  local bytearr = {}
  for _, v in ipairs(t) do
    local utf8byte = v < 0 and (0xff + v + 1) or v
    table.insert(bytearr, string.char(utf8byte))
  end
  return table.concat(bytearr)
end

return M
