function capture(cmd, raw)
	local handle = assert(io.popen(cmd, 'r'))
	local output = assert(handle:read('*a'))

	handle:close()

	if raw then
		return output
	end

	output = string.gsub(string.gsub(string.gsub(output, '^%s+', ''), '%s+$', ''), '[\n\r]+', ' ')

	return output -- == '' and nil or output
end

-- local handle = capture('pactl get-sink-mute @DEFAULT_SINK@ | grep yes')
local handle = capture('pamixer --get-volume-human')
-- if not handle then
-- 	return 1
-- end
-- local result = handle:read('*a') or 'Nothing found'
-- print(string.len(handle) > 0 and handle or 'Unmute')
print(string.format('%s%%', handle))
-- handle:close()
