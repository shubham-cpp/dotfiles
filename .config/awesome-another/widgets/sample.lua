-- local function get_relative_path(file_a_location, rc_lua_location)
--     local file_a_rel_path = vim.loop.fs_realpath(file_a_location)
--     local rc_lua_rel_path = vim.loop.fs_realpath(rc_lua_location)
--
--     local file_a_dir = vim.fn.fnamemodify(file_a_rel_path, ":h")
--     local rc_lua_dir = vim.fn.fnamemodify(rc_lua_rel_path, ":h")
--
--     local file_a_dir_components = {}
--     for component in file_a_dir:gmatch("[^/]+") do
--         table.insert(file_a_dir_components, component)
--     end
--
--     local rc_lua_dir_components = {}
--     for component in rc_lua_dir:gmatch("[^/]+") do
--         table.insert(rc_lua_dir_components, component)
--     end
--
--     local relative_path = ""
--     local num_common_dirs = 0
--     while num_common_dirs < #file_a_dir_components and num_common_dirs < #rc_lua_dir_components do
--         if file_a_dir_components[num_common_dirs + 1] == rc_lua_dir_components[num_common_dirs + 1] then
--             num_common_dirs = num_common_dirs + 1
--         else
--             break
--         end
--     end
--
--     for i = num_common_dirs + 1, #rc_lua_dir_components do
--         relative_path = relative_path .. "../"
--     end
--
--     for i = num_common_dirs + 1, #file_a_dir_components do
--         relative_path = relative_path .. file_a_dir_components[i] .. "/"
--     end
--
--     relative_path = relative_path .. vim.fn.fnamemodify(file_a_rel_path, ":t")
--
--     return relative_path
-- end
--
-- local file_a_location =
-- "/home/shubham/Documents/Programming/WebDev/Astro/astro-project-manager/server/src/utils/validations/index.ts"
-- local rc_lua_location = vim.fn.expand("%:p")
-- -- "/home/shubham/Documents/Programming/WebDev/Astro/astro-project-manager/server/src/routes/User.ts"
--
-- local relative_path = get_relative_path(file_a_location, rc_lua_location)
--
-- print(relative_path)
-- local volume_get_command = "wpctl get-volume @DEFAULT_AUDIO_SINK@"
-- local naughty = require("naughty")
-- require("awful").spawn.easy_async_with_shell(volume_get_command, function(stdout)
-- 	naughty.notify({ text = "LINE:" .. stdout })
-- end)

local volume = "Volume: 0.80 [MUTED]"
local volume_set_command = "wpctl set-volume @DEFAULT_AUDIO_SINK@ %s"
-- print(volume_set_command .. "5%-")
print(string.format(volume_set_command, "5%-"))