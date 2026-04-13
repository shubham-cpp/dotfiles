local function trash(state)
  local inputs = require("neo-tree.ui.inputs")
  local node = state.tree:get_node()
  if not node or node.type == "message" then
    return
  end

  local _, name = require("neo-tree.utils").split_path(node.path)
  local msg = string.format("Are you sure you want to trash '%s'?", name)
  inputs.confirm(msg, function(confirmed)
    if not confirmed then
      return
    end
    vim.system({ "trash", node.path }):wait()
    require("neo-tree.sources.manager").refresh(state)
  end)
end

local function trash_visual(state, selected_nodes)
  local inputs = require("neo-tree.ui.inputs")
  local paths_to_trash = {}

  for _, node in ipairs(selected_nodes) do
    if node.type ~= "message" then
      table.insert(paths_to_trash, node.path)
    end
  end

  if #paths_to_trash == 0 then
    return
  end

  local msg = "Are you sure you want to trash " .. #paths_to_trash .. " items?"
  inputs.confirm(msg, function(confirmed)
    if not confirmed then
      return
    end
    for _, path in ipairs(paths_to_trash) do
      vim.system({ "trash", path }):wait()
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
    ["PATH (CWD)"] = modify(filepath, ":."),
    ["PATH (HOME)"] = modify(filepath, ":~"),
    ["URI"] = vim.uri_from_fname(filepath),
  }

  local options = vim.tbl_filter(function(val)
    return vals[val] ~= ""
  end, vim.tbl_keys(vals))

  if vim.tbl_isempty(options) then
    vim.notify("No values to copy", vim.log.levels.WARN)
    return
  end

  table.sort(options)
  vim.ui.select(options, {
    prompt = "Choose to copy to clipboard:",
    format_item = function(item)
      return ("%s: %s"):format(item, vals[item])
    end,
  }, function(choice)
    local result = vals[choice]
    if result then
      vim.notify(("Copied: `%s`"):format(result))
      vim.fn.setreg("+", result)
    end
  end)
end

local function system_open(state)
  local node = state.tree:get_node()
  vim.ui.open(node:get_id())
end

local function child_or_open(state)
  local node = state.tree:get_node()
  if node:has_children() then
    if not node:is_expanded() then
      state.commands.toggle_node(state)
    elseif node.type == "file" then
      state.commands.open(state)
    else
      require("neo-tree.ui.renderer").focus_node(state, node:get_child_ids()[1])
    end
  else
    state.commands.open(state)
  end
end

local function parent_or_close(state)
  local node = state.tree:get_node()
  if node:has_children() and node:is_expanded() then
    state.commands.toggle_node(state)
  else
    require("neo-tree.ui.renderer").focus_node(state, node:get_parent_id())
  end
end

local function collapse_or_open(state)
  local node = state.tree:get_node()
  if node:has_children() then
    if not node:is_expanded() then
      state.commands.toggle_node(state)
    elseif node.type == "file" then
      state.commands.open(state)
    else
      state.commands.toggle_node(state)
    end
  else
    state.commands.open(state)
  end
end

return {
  { url = "MunifTanjim/nui.nvim" },
  { url = "antosha417/nvim-lsp-file-operations" },
  {
    url = "nvim-neo-tree/neo-tree.nvim",
    config = function()
      local lsp_file_operations = require("core.lsp_file_operations")

      require("neo-tree").setup({
        close_if_last_window = true,
        window = {
          position = "right",
          insert_as = "sibling",
          mappings = {
            ["<space>"] = "none",
          },
        },
        commands = {
          trash = trash,
          trash_visual = trash_visual,
          copy_selector = copy_selector,
          system_open = system_open,
          parent_or_close = parent_or_close,
          child_or_open = child_or_open,
          collapse_or_open = collapse_or_open,
        },
        filesystem = {
          bind_to_cwd = false,
          follow_current_file = {
            enabled = true,
          },
          hijack_netrw_behavior = "disabled",
          use_libuv_file_watcher = true,
          window = {
            mappings = {
              ["d"] = "trash",
              ["D"] = "delete",
              ["h"] = "parent_or_close",
              ["Y"] = "copy_selector",
              ["o"] = "system_open",
              ["l"] = "collapse_or_open",
              ["L"] = "child_or_open",
            },
          },
        },
      })

      require("lsp-file-operations").setup({
        operations = lsp_file_operations.operations,
      })
    end,
    keys = {
      {
        "<leader>e",
        function()
          require("neo-tree.command").execute({
            toggle = true,
            source = "filesystem",
            dir = vim.uv.cwd(),
            reveal = true,
          })
        end,
        desc = "Toggle Neo-tree",
      },
      {
        "<leader>E",
        function()
          require("neo-tree.command").execute({
            toggle = true,
            source = "filesystem",
            dir = vim.uv.cwd(),
          })
        end,
        desc = "Toggle Neo-tree",
      },
      {
        "<leader>oe",
        "<cmd>Neotree focus<CR>",
        desc = "Toggle Neo-tree",
      },
    },
  },
}
