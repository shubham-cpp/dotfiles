if type(vim.pack) ~= "table" then
  return
end

local function inactive_pack_plugins()
  local plugins = vim.pack.get(nil, { info = false })
  local names = {}

  for _, plugin in ipairs(plugins) do
    if not plugin.active then
      table.insert(names, plugin.spec.name)
    end
  end

  table.sort(names)

  return names
end

vim.api.nvim_create_user_command("PlugClean", function(opts)
  local names = inactive_pack_plugins()

  if vim.tbl_isempty(names) then
    vim.notify("No inactive vim.pack plugins found", vim.log.levels.INFO)
    return
  end

  if not opts.bang then
    local lines = {
      ("Delete %d inactive vim.pack plugin(s)?"):format(#names),
      "",
      table.concat(names, "\n"),
    }
    local choice = vim.fn.confirm(table.concat(lines, "\n"), "&Yes\n&No", 2)
    if choice ~= 1 then
      vim.notify("PlugClean cancelled", vim.log.levels.INFO)
      return
    end
  end

  vim.pack.del(names)
  vim.notify(("Deleted %d inactive vim.pack plugin(s)"):format(#names), vim.log.levels.INFO)
end, {
  bang = true,
  desc = "Delete vim.pack plugins not active in the current session",
})

require "plugins.treesitter"
require "plugins.mini"
require "plugins.workspace-session"
require "plugins.blink"
require "plugins.lsp"
require "plugins.multicursor"
require "plugins.conform"
require "plugins.nvim-lint"
require "plugins.color-highlight"
require "plugins.snacks"
require "plugins.overseer"
require "plugins.neogit"
require "plugins.neogen"
