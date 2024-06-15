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
      local map_split = function(buf_id, lhs, direction, close_on_file)
        local rhs = function()
          local new_target_window
          local cur_target_window = require('mini.files').get_target_window()
          if cur_target_window ~= nil then
            vim.api.nvim_win_call(cur_target_window, function()
              vim.cmd('belowright ' .. direction .. ' split')
              new_target_window = vim.api.nvim_get_current_win()
            end)

            require('mini.files').set_target_window(new_target_window)
            require('mini.files').go_in({ close_on_file = close_on_file })
          end
        end

        local desc = 'Open in ' .. direction .. ' split'
        if close_on_file then
          desc = desc .. ' and close'
        end
        vim.keymap.set('n', lhs, rhs, { buffer = buf_id, desc = desc })
      end

      vim.api.nvim_create_autocmd('User', {
        pattern = 'MiniFilesBufferCreate',
        group = au_group,
        desc = 'Open in split(gs) or vsplit(gv)',
        callback = function(args)
          local buf_id = args.data.buf_id
          -- Tweak keys to your liking
          map_split(buf_id, 'gs', 'horizontal', false)
          map_split(buf_id, 'gv', 'vertical', false)
          map_split(buf_id, 'gS', 'horizontal', true)
          map_split(buf_id, 'gV', 'vertical', true)
        end,
      })
      vim.api.nvim_create_autocmd('User', {
        pattern = 'MiniFilesActionRename',
        group = au_group,
        desc = 'LSP Rename file',
        callback = function(event)
          require('plugins.utils.lsp').on_rename(event.data.from, event.data.to)
        end,
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
