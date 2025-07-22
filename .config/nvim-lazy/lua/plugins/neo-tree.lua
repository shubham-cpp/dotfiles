-- Trash the target
local function trash(state)
  local inputs = require("neo-tree.ui.inputs")
  local node = state.tree:get_node()
  if node.type == "message" then
    return
  end
  local _, name = require("neo-tree.utils").split_path(node.path)
  local msg = string.format("Are you sure you want to trash '%s'?", name)
  inputs.confirm(msg, function(confirmed)
    if not confirmed then
      return
    end
    vim.api.nvim_command("silent !trash " .. node.path)
    require("neo-tree.sources.manager").refresh(state)
  end)
end

-- Trash the selections (visual mode)
local function trash_visual(state, selected_nodes)
  local inputs = require("neo-tree.ui.inputs")
  local paths_to_trash = {}
  for _, node in ipairs(selected_nodes) do
    if node.type ~= "message" then
      table.insert(paths_to_trash, node.path)
    end
  end
  local msg = "Are you sure you want to trash " .. #paths_to_trash .. " items?"
  inputs.confirm(msg, function(confirmed)
    if not confirmed then
      return
    end
    for _, path in ipairs(paths_to_trash) do
      vim.api.nvim_command("silent !trash " .. path)
    end
    require("neo-tree.sources.manager").refresh(state)
  end)
end

---@type LazySpec
return {
  "nvim-neo-tree/neo-tree.nvim",
  optional = true,
  opts = {
    commands = {
      trash = trash,
      trash_visual = trash_visual,
    },
    window = {
      mappings = {
        ["d"] = "trash",
        ["D"] = "delete",
        ["F"] = "fuzzy_finder_directory",
      },
    },
    filesystem = {
      commands = {
        system_open = function(state)
          vim.ui.open(state.tree:get_node():get_id())
        end,
        parent_or_close = function(state)
          local node = state.tree:get_node()
          if node:has_children() and node:is_expanded() then
            state.commands.toggle_node(state)
          else
            require("neo-tree.ui.renderer").focus_node(state, node:get_parent_id())
          end
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
        position = "right", -- left, right, top, bottom, float, current
        width = 45,
        mappings = {
          O = "system_open",
          ["<S-CR>"] = "system_open",
          ["[b"] = "prev_source",
          ["]b"] = "next_source",
          L = "child_or_open",
          l = "collapse_or_open",
          h = "parent_or_close",
        },
        fuzzy_finder_mappings = { -- define keymaps for filter popup window in fuzzy_finder_mode
          ["<C-J>"] = "move_cursor_down",
          ["<C-K>"] = "move_cursor_up",
        },
      },
    },
  },
}
