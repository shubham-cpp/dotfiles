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
---@type LazySpec
return {
  {
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
        },
      },
      filesystem = {
        commands = {
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
            ["l"] = "collapse_or_open",
            ["L"] = "child_or_open",
            ["F"] = "fuzzy_finder_directory",
          },
        },
      },
    },
  },
  {
    "mikavilpas/yazi.nvim",
    cmd = "Yazi",
    dependencies = { "folke/snacks.nvim" },
    keys = {
      -- 👇 in this section, choose your own keymappings!
      {
        "<leader>-",
        mode = { "n", "v" },
        "<cmd>Yazi<cr>",
        desc = "Open yazi at the current file",
      },
      {
        -- Open in the current working directory
        "<leader>_",
        "<cmd>Yazi cwd<cr>",
        desc = "Open the file manager in nvim's working directory",
      },
    },
    ---@type YaziConfig | {}
    opts = {
      -- if you want to open yazi instead of netrw, see below for more info
      open_for_directories = false,
      keymaps = {
        show_help = "<f1>",
      },
      ---@type 'none'| 'rounded'| 'single'| 'double'| 'shadow'
      yazi_floating_window_border = "rounded",
    },
    -- 👇 if you use `open_for_directories=true`, this is recommended
    init = function()
      -- More details: https://github.com/mikavilpas/yazi.nvim/issues/802
      -- vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1
    end,
  },
}
