--- if current buffer is a neotree buffer, toggle it
--- if we're currently not in focused for neotree then focus on the neotree buffer
local function better_toggle_neotree()
  local bufnr = vim.api.nvim_get_current_buf()
  local bufname = vim.fn.bufname(bufnr)
  if bufname ~= '' and string.match(bufname, 'neo.tree %w+') ~= nil then
    vim.cmd 'Neotree toggle'
  else
    vim.cmd 'Neotree reveal focus'
  end
end
local cmd = 'trash'
if vim.fn.executable 'trash-put' == 1 then
  cmd = 'trash-put'
end

---@type LazySpec
return {
  'nvim-neo-tree/neo-tree.nvim',
  version = 'v3.x',
  cmd = 'Neotree',
  enabled = true,
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
    'antosha417/nvim-lsp-file-operations',
    {
      's1n7ax/nvim-window-picker',
      version = '2.*',
      config = function()
        require('window-picker').setup({
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
          --- @type 'statusline-winbar' | 'floating-big-letter'
          hint = 'floating-big-letter',
        })
      end,
    },
  },
  keys = {
    {
      '<leader>e',
      better_toggle_neotree,
      desc = 'Open File [E]xplorer(Reveal)',
    },
    { '<leader>E', '<cmd>Neotree focus<cr>', desc = 'Open File [E]xplorer' },
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
  },
  config = function(_, opts)
    require('neo-tree').setup(opts)
    require('lsp-file-operations').setup({})
  end,
}
