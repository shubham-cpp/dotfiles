local au_group = vim.api.nvim_create_augroup('sp_mini', { clear = true })
return {
  {
    'echasnovski/mini.files',
    version = '*',
    keys = function()
      local f = require 'mini.files'
      local minifiles_toggle = function(...)
        if not f.close() then
          f.open(...)
        end
      end
      return {
        {
          '<leader>e',
          function()
            minifiles_toggle(vim.api.nvim_buf_get_name(0), false)
          end,
          desc = 'Open file explorer(same directory as current file)',
        },
        {
          '<leader>E',
          minifiles_toggle,
          desc = 'Open file explorer',
        },
      }
    end,
    config = function()
      require('mini.files').setup({
        options = {
          permanent_delete = false,
          use_as_default_explorer = true,
        },
        windows = { preview = true, width_preview = 45 },
      })
      local map_split = function(buf_id, lhs, direction)
        local rhs = function()
          -- Make new window and set it as target
          local new_target_window
          vim.api.nvim_win_call(MiniFiles.get_target_window(), function()
            vim.cmd(direction .. ' split')
            new_target_window = vim.api.nvim_get_current_win()
          end)

          MiniFiles.set_target_window(new_target_window)
        end

        -- Adding `desc` will result into `show_help` entries
        local desc = 'Split ' .. direction
        vim.keymap.set('n', lhs, rhs, { buffer = buf_id, desc = desc })
      end

      vim.api.nvim_create_autocmd('User', {
        pattern = 'MiniFilesBufferCreate',
        group = au_group,
        callback = function(args)
          local buf_id = args.data.buf_id
          -- Tweak keys to your liking
          map_split(buf_id, 'gs', 'belowright horizontal')
          map_split(buf_id, 'gv', 'belowright vertical')
        end,
        desc = 'Open in split(gs) or vsplit(gv)',
      })
    end,
  },
  {
    'echasnovski/mini.base16',
    enabled = false,
    version = false,
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd [[
        colorscheme myscheme
      ]]
    end,
  },
}
