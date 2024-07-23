local cmd = "trash"
if vim.fn.executable("trash-put") == 1 then
  cmd = "trash-put"
end
---@type LazySpec
return {
  "nvim-neo-tree/neo-tree.nvim",
  opts = function(_, opts)
    opts.window.mappings["l"] = "open"
    -- opts.window.mappings['l'] = 'open'

    opts.filesystem.commands = vim.tbl_extend("force", opts.filesystem.commands or {}, {
      ["delete"] = function(state)
        local inputs = require("neo-tree.ui.inputs")
        local path = state.tree:get_node().path
        local msg = "Are you sure you want to trash " .. path
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
        local msg = "Are you sure you want to trash " .. count .. " files ?"
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
