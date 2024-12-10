local M = {}

M.config = function(_, opts)
  local cmd = 'trash'
  if vim.fn.executable 'trash-put' == 1 then
    cmd = 'trash-put'
  end

  local options = vim.tbl_deep_extend('force', {
    window = {
      mappings = {
        l = 'open',
        ['?'] = 'show_help',
        ['<'] = 'prev_source',
        ['>'] = 'next_source',
      },
    },
    filesystem = {
      use_libuv_file_watcher = true,
      window = {
        -- mappings = { },
        fuzzy_finder_mappings = {
          ['<down>'] = 'move_cursor_down',
          ['<up>'] = 'move_cursor_up',
          ['<C-j>'] = 'move_cursor_down',
          ['<C-k>'] = 'move_cursor_up',
        },
      },

      commands = {
        -- Override delete to use trash instead of rm
        delete = function(state)
          local inputs = require 'neo-tree.ui.inputs'
          local path = state.tree:get_node().path
          local msg = 'Trash ' .. path .. ' ?'
          inputs.confirm(msg, function(confirmed)
            if not confirmed then
              return
            end

            vim.fn.system({ cmd, vim.fn.fnameescape(path) })
            require('neo-tree.sources.manager').refresh(state.name)
          end)
        end,
        -- over write default 'delete_visual' command to 'trash' x n.
        delete_visual = function(state, selected_nodes)
          local inputs = require 'neo-tree.ui.inputs'
          local count = #selected_nodes
          local msg = 'Trash ' .. count .. ' files ?'
          inputs.confirm(msg, function(confirmed)
            if not confirmed then
              return
            end
            for _, node in ipairs(selected_nodes) do
              vim.fn.system({ cmd, vim.fn.fnameescape(node.path) })
            end
            require('neo-tree.sources.manager').refresh(state.name)
          end)
        end,
      },
    },
  }, opts)

  require('neo-tree').setup(options)
  require('lsp-file-operations').setup({})
end
return M
