return {
  'nvim-neo-tree/neo-tree.nvim',
  version = 'v3.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
    {
      's1n7ax/nvim-window-picker',
      name = 'window-picker',
      version = '2.*',
      lazy = true,
      opts = {
        -- 'statusline-winbar' | 'floating-big-letter'
        hint = 'floating-big-letter',
        filter_rules = {
          include_current_win = false,
          autoselect_one = true,
          -- filter using buffer options
          bo = {
            -- if the file type is one of following, the window will be ignored
            filetype = { 'neo-tree', 'neo-tree-popup', 'notify' },
            -- if the buffer type is one of following, the window will be ignored
            buftype = { 'terminal', 'quickfix' },
          },
        },
      },
    },
  },
  keys = {
    { '<leader>e', '<cmd>Neotree reveal toggle<cr>', desc = 'Open File [E]xplorer(Reveal)' },
    { '<leader>E', '<cmd>Neotree focus toggle<cr>', desc = 'Open File [E]xplorer' },
  },
  init = function()
    vim.g.neo_tree_remove_legacy_commands = 1
  end,
  opts = {
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
