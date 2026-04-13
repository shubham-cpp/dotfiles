-- Plugin loader: auto-discovers lua/plugins/*.lua specs and registers with vim.pack
local plugin_dir = vim.fn.stdpath("config") .. "/lua/plugins"
local files = vim.fs.dir(plugin_dir)

local specs = {}
for name, type in files do
  if type == "file" and name:match("%.lua$") and name ~= "init.lua" then
    local modname = "plugins." .. name:gsub("%.lua$", "")
    local ok, plugin_specs = pcall(require, modname)
    if ok then
      -- Normalize single spec to list
      if plugin_specs.url then
        plugin_specs = { plugin_specs }
      end
      for _, spec in ipairs(plugin_specs) do
        table.insert(specs, spec)
      end
    else
      vim.notify(("Failed to load plugin spec %s: %s"):format(name, plugin_specs), vim.log.levels.ERROR)
    end
  end
end

-- Convert our spec format to vim.pack.add() format
local pack_specs = {}
for _, spec in ipairs(specs) do
  local src = spec.url
  if src and not src:match("^https?://") then
    src = "https://github.com/" .. src
  end
  local pack_spec = { src = src }
  if spec.version then
    pack_spec.version = spec.version
  end
  table.insert(pack_specs, pack_spec)
end

vim.pack.add(pack_specs)

-- Process config and keys for each plugin
for _, spec in ipairs(specs) do
  if spec.config then
    local ok, err = pcall(spec.config)
    if not ok then
      vim.notify(("Plugin config error (%s): %s"):format(spec.url, err), vim.log.levels.ERROR)
    end
  end
  if spec.keys then
    local keys = type(spec.keys) == "function" and spec.keys() or spec.keys
    for _, key in ipairs(keys) do
      if type(key) == "string" then
        -- String shorthand: just load plugin when this key is pressed (no mapping set)
        -- The plugin itself defines the mapping
      elseif type(key) == "table" and key[1] and key[2] then
        vim.keymap.set(key.mode or "n", key[1], key[2], { desc = key.desc })
      end
    end
  end
end
