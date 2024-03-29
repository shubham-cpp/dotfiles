return {
  'nvim-neo-tree/neo-tree.nvim',
  dependencies = {
    {
      's1n7ax/nvim-window-picker',
      name = 'window-picker',
      -- event = 'VeryLazy',
      lazy = true,
      version = '2.*',
      opts = {
        -- 'statusline-winbar' | 'floating-big-letter'
        hint = 'floating-big-letter',
      },
    },
  },
  opts = {
    window = {
      mappings = {
        ['l'] = 'open',
        ['<'] = 'prev_source',
        ['>'] = 'next_source',
      },
    },
    filesystem = {
      commands = {
        -- Override delete to use trash instead of rm
        delete = function(state)
          local inputs = require 'neo-tree.ui.inputs'
          local path = state.tree:get_node().path
          local msg = 'Are you sure you want to trash ' .. path
          inputs.confirm(msg, function(confirmed)
            if not confirmed then
              return
            end

            vim.fn.system({ 'trash-put', vim.fn.fnameescape(path) })
            require('neo-tree.sources.manager').refresh(state.name)
          end)
        end,
        delete_visual = function(state, selected_nodes)
          local inputs = require 'neo-tree.ui.inputs'
          local count = #selected_nodes
          local msg = 'Are you sure you want to trash ' .. count .. ' files ?'
          inputs.confirm(msg, function(confirmed)
            if not confirmed then
              return
            end
            for _, node in ipairs(selected_nodes) do
              vim.fn.system({ 'trash-put', vim.fn.fnameescape(node.path) })
            end
            require('neo-tree.sources.manager').refresh(state.name)
          end)
        end,
      },
    },
  },
}
