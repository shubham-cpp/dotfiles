local cmd = "trash"
if vim.fn.executable("trash-put") == 1 then
  cmd = "trash-put"
end
--- if current buffer is a neotree buffer, toggle it
--- if we're currently not in focused for neotree then focus on the neotree buffer
local function better_toggle_neotree()
  local bufnr = vim.api.nvim_get_current_buf()
  local bufname = vim.fn.bufname(bufnr)
  if bufname ~= "" and string.match(bufname, "neo.tree %w+") ~= nil then
    vim.cmd("Neotree toggle")
  else
    vim.cmd("Neotree reveal focus")
  end
end

---@type LazySpec
return {
  "nvim-neo-tree/neo-tree.nvim",
  keys = {
    { "<leader>e", better_toggle_neotree, desc = "[E]xplorer neotree" },
  },
  dependencies = {
    {
      "antosha417/nvim-lsp-file-operations",
      config = function()
        LazyVim.on_load("neo-tree.nvim", function()
          require("lsp-file-operations").setup({})
        end)
      end,
    },
    {
      "s1n7ax/nvim-window-picker",
      version = "2.*",
      config = function()
        require("window-picker").setup({
          filter_rules = {
            include_current_win = false,
            autoselect_one = true,
            -- filter using buffer options
            bo = {
              -- if the file type is one of following, the window will be ignored
              filetype = { "neo-tree", "neo-tree-popup", "notify" },
              -- if the buffer type is one of following, the window will be ignored
              buftype = { "terminal", "quickfix" },
            },
          },
          --- @type 'statusline-winbar' | 'floating-big-letter'
          hint = "floating-big-letter",
        })
      end,
    },
  },

  opts = function(_, opts)
    opts.window.mappings["l"] = "open"

    opts.filesystem.commands = vim.tbl_extend("force", opts.filesystem.commands or {}, {
      ["delete"] = function(state)
        local inputs = require("neo-tree.ui.inputs")
        local path = state.tree:get_node().path
        local msg = "Trash " .. path
        inputs.confirm(msg, function(confirmed)
          if not confirmed then
            return
          end

          vim.fn.system({ cmd, vim.fn.fnameescape(path) })
          require("neo-tree.sources.manager").refresh(state.name)
        end)
      end,
      ["delete_visual"] = function(state, selected_nodes)
        local inputs = require("neo-tree.ui.inputs")
        local count = #selected_nodes
        local msg = "Trash " .. count .. " files ?"
        inputs.confirm(msg, function(confirmed)
          if not confirmed then
            return
          end
          for _, node in ipairs(selected_nodes) do
            vim.fn.system({ cmd, vim.fn.fnameescape(node.path) })
          end
          require("neo-tree.sources.manager").refresh(state.name)
        end)
      end,
    })
    return opts
  end,
}
