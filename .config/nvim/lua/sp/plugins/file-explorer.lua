return {
    'nvim-neo-tree/neo-tree.nvim',
    -- branch = 'v2.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
      'MunifTanjim/nui.nvim',
      {
        's1n7ax/nvim-window-picker',
        version = '*',
        config = function()
          require('window-picker').setup({
            autoselect_one = true,
            include_current = false,
            filter_rules = {
              -- filter using buffer options
              bo = {
                -- if the file type is one of following, the window will be ignored
                filetype = { 'neo-tree', 'neo-tree-popup', 'notify' },
                -- if the buffer type is one of following, the window will be ignored
                buftype = { 'terminal', 'quickfix' },
              },
            },
            other_win_hl_color = '#e35e4f',
          })
        end,
      },
    },
    keys = {
      { '<leader>e', '<cmd>Neotree focus toggle<cr>', desc = 'Open File [E]xplorer' },
    },
    init = function ()
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
      }
    },
  }
