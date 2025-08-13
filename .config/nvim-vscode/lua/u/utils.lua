local M = {}
-- Async file lister for Neovim using vim.loop (libuv)
local uv = vim.loop

--- Walk through files in a directory asynchronously.
--- @param dir string Directory path
--- @param callback fun(name:string, type:string) `name` of the file/directory; `type` is either file or directory
function M.walk_files(dir, callback)
  uv.fs_opendir(dir, function(err, dir_handle)
    if err then
      vim.schedule(function()
        vim.notify("Error opening dir: " .. err, vim.log.levels.ERROR)
      end)
      return
    end
    local function read_entries()
      uv.fs_readdir(dir_handle, function(rerr, entries)
        if rerr then
          vim.schedule(function()
            vim.notify("Error reading dir: " .. rerr, vim.log.levels.ERROR)
          end)
          dir_handle:closedir()
          return
        end

        if not entries then
          dir_handle:closedir()
          return
        end

        for _, entry in ipairs(entries) do
          vim.schedule(function()
            callback(entry.name, entry.type) -- type can be "file", "directory", "link", etc.
          end)
        end

        -- Keep reading until no more entries
        read_entries()
      end)
    end

    read_entries()
  end)
end

---add prefix '<cmd>' and suffix '<cr>'
---@param cmd_name string
---@return string
function M.cmd_str(cmd_name)
  return "<cmd>" .. cmd_name .. "<cr>"
end

return M
