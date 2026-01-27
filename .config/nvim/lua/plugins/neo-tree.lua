-- Trash the target
local function trash(state)
  local inputs = require "neo-tree.ui.inputs"
  local node = state.tree:get_node()
  if node.type == "message" then return end
  local _, name = require("neo-tree.utils").split_path(node.path)
  local msg = string.format("Are you sure you want to trash '%s'?", name)
  inputs.confirm(msg, function(confirmed)
    if not confirmed then return end
    vim.api.nvim_command("silent !trash " .. node.path)
    require("neo-tree.sources.manager").refresh(state)
  end)
end

-- Trash the selections (visual mode)
local function trash_visual(state, selected_nodes)
  local inputs = require "neo-tree.ui.inputs"
  local paths_to_trash = {}
  for _, node in ipairs(selected_nodes) do
    if node.type ~= "message" then table.insert(paths_to_trash, node.path) end
  end
  local msg = "Are you sure you want to trash " .. #paths_to_trash .. " items?"
  inputs.confirm(msg, function(confirmed)
    if not confirmed then return end
    for _, path in ipairs(paths_to_trash) do
      vim.api.nvim_command("silent !trash " .. path)
    end
    require("neo-tree.sources.manager").refresh(state)
  end)
end

local function copy_selector(state)
  local node = state.tree:get_node()
  local filepath = node:get_id()
  local filename = node.name
  local modify = vim.fn.fnamemodify

  local vals = {
    ["FILENAME"] = filename,
    ["BASENAME"] = modify(filename, ":r"),
    -- ["EXTENSION"] = modify(filename, ":e"),
    ["PATH (CWD)"] = modify(filepath, ":."),
    ["PATH (HOME)"] = modify(filepath, ":~"),
    ["URI"] = vim.uri_from_fname(filepath),
  }

  local options = vim.tbl_filter(function(val) return vals[val] ~= "" end, vim.tbl_keys(vals))
  if vim.tbl_isempty(options) then
    vim.notify("No values to copy", vim.log.levels.WARN)
    return
  end
  table.sort(options)
  vim.ui.select(options, {
    prompt = "Choose to copy to clipboard:",
    format_item = function(item) return ("%s: %s"):format(item, vals[item]) end,
  }, function(choice)
    local result = vals[choice]
    if result then
      vim.notify(("Copied: `%s`"):format(result))
      vim.fn.setreg("+", result)
    end
  end)
end

---@type LazySpec
return {
  "nvim-neo-tree/neo-tree.nvim",
  opts = {
    window = {
      position = "right",
      ---@type "child"|"sibling"
      insert_as = "sibling",
      mappings = {
        ["d"] = "trash",
        ["D"] = "delete",
        ["Y"] = "copy_selector",
      },
      use_libuv_file_watcher = true,
    },
    commands = {
      trash = trash,
      trash_visual = trash_visual,
      copy_selector = copy_selector,
    },
    filesystem = {
      commands = {
        system_open = function(state)
          local node = state.tree:get_node()
          local path = node:get_id()
          vim.ui.open(path)
        end,
        collapse_or_open = function(state)
          local node = state.tree:get_node()
          if node:has_children() then
            if not node:is_expanded() then -- if unexpanded, expand
              state.commands.toggle_node(state)
            else -- if expanded and has children, seleect the next child
              if node.type == "file" then
                state.commands.open(state)
              else
                state.commands.toggle_node(state)
              end
            end
          else -- if has no children
            state.commands.open(state)
          end
        end,
      },
      window = {
        mappings = {
          ["F"] = "fuzzy_finder_directory",
          ["o"] = "system_open",
          ["l"] = "collapse_or_open",
          ["L"] = "child_or_open",
        },
      },
    },
  },
}
