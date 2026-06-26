local function trash(state)
  local inputs = require "neo-tree.ui.inputs"
  local node = state.tree:get_node()
  if not node or node.type == "message" then return end
  local _, name = require("neo-tree.utils").split_path(node.path)
  local msg = string.format("Are you sure you want to trash '%s'?", name)
  inputs.confirm(msg, function(confirmed)
    if not confirmed then return end
    local os_name = vim.uv.os_uname().sysname
    if os_name == "Linux" then
      vim.system({ "gio", "trash", node.path }):wait()
    else
      -- Macos
      vim.system({ "trash", node.path }):wait()
    end
    require("neo-tree.sources.manager").refresh(state)
  end)
end

local function trash_visual(state, selected_nodes)
  local inputs = require "neo-tree.ui.inputs"
  local paths_to_trash = {}
  for _, node in ipairs(selected_nodes) do
    if node.type ~= "message" then table.insert(paths_to_trash, node.path) end
  end
  if #paths_to_trash == 0 then return end
  local msg = "Are you sure you want to trash " .. #paths_to_trash .. " items?"
  inputs.confirm(msg, function(confirmed)
    if not confirmed then return end
    local os_name = vim.uv.os_uname().sysname
    for _, path in ipairs(paths_to_trash) do
      if os_name == "Linux" then
        vim.system({ "gio", "trash", path }):wait()
      else
        -- Macos
        vim.system({ "trash", path }):wait()
      end
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

local function collapse_or_open(state)
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
        copy_selector = copy_selector,
      },
      window = {
        mappings = {
          ["d"] = "trash",
          ["D"] = "delete",
        },
      },
      filesystem = {
        commands = {
          collapse_or_open = collapse_or_open,
        },
        window = {
          position = "right", -- left, right, top, bottom, float, current
          width = 45,
          mappings = {
            ["l"] = "collapse_or_open",
            ["L"] = "child_or_open",
            ["F"] = "fuzzy_finder_directory",
            ["Y"] = "copy_selector",
          },
        },
      },
    },
  },
  {
    "mikavilpas/yazi.nvim",
    cmd = "Yazi",
    dependencies = {
      { "nvim-lua/plenary.nvim", lazy = true },
      {
        "AstroNvim/astrocore",
        ---@type AstroCoreOpts
        opts = {
          mappings = {
            n = {
              ["<Leader>-"] = { "<Cmd>Yazi<CR>", desc = "Open yazi at the current file" },
              ["<Leader>_"] = { "<Cmd>Yazi cwd<CR>", desc = "Open yazi in nvim's working directory" },
              ["<Leader>uY"] = { "<Cmd>Yazi toggle<CR>", desc = "Resume the last yazi session" },
            },
            v = {
              ["<Leader>-"] = { "<Cmd>Yazi<CR>", desc = "Open yazi at the current file" },
            },
          },
        },
      },
    },
    ---@type YaziConfig | {}
    opts = {
      open_for_directories = false,
      keymaps = {
        show_help = "<f1>",
      },
      ---@type 'none'| 'rounded'| 'single'| 'double'| 'shadow'
      yazi_floating_window_border = "rounded",
      integrations = {
        grep_in_directory = "fzf-lua",
        grep_in_selected_files = "fzf-lua",
      },
    },
  },
}
