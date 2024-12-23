local au_group = vim.api.nvim_create_augroup('sp_mini', { clear = true })

local minifiles_toggle = function(...)
  local f = require 'mini.files'
  local arg_len = ... == nil and 0 or #...
  if not f.close() then
    if arg_len == 0 then
      f.open(f.get_latest_path())
    else
      f.open(...)
    end
  end
end

---@type LazySpec
return {
  'echasnovski/mini.files',
  dependencies = {
    'nvim-tree/nvim-web-devicons',
    { 'antosha417/nvim-lsp-file-operations', dependencies = { 'nvim-lua/plenary.nvim' } },
  },
  version = '*',
  enabled = false,
  keys = {
    {
      '<leader>e',
      minifiles_toggle,
      desc = 'Toggle file explorer',
    },
    {
      '<leader>E',
      function()
        minifiles_toggle(vim.api.nvim_buf_get_name(0), false)
      end,
      desc = 'Toggle file explorer(current file)',
    },
  },
  config = function()
    require('mini.files').setup({
      options = {
        permanent_delete = false,
        use_as_default_explorer = true,
      },
      windows = { preview = true, width_preview = 45 },
    })
    require('lsp-file-operations').setup()

    local map_split = function(buf_id, lhs, direction, close_on_file)
      local rhs = function()
        local new_target_window
        local cur_target_window = require('mini.files').get_explorer_state().target_window
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

        map_split(buf_id, '<C-w>s', 'horizontal', false)
        map_split(buf_id, '<C-w>v', 'vertical', false)
        map_split(buf_id, '<C-w>S', 'horizontal', true)
        map_split(buf_id, '<C-w>V', 'vertical', true)
      end,
    })
    local events = {
      -- ['lsp-file-operations.did-rename'] = { { 'MiniFilesActionRename', 'MiniFilesActionMove' }, 'Renamed' },
      ['lsp-file-operations.will-create'] = { 'MiniFilesActionCreate', 'Create' },
      ['lsp-file-operations.will-delete'] = { 'MiniFilesActionDelete', 'Delete' },
    }
    for module, pattern in pairs(events) do
      vim.api.nvim_create_autocmd('User', {
        pattern = pattern[1],
        group = au_group,
        desc = string.format('Auto-refactor LSP file %s', pattern[2]),
        callback = function(event)
          local ok, action = pcall(require, module)
          if not ok then
            return
          end
          local args = {}
          local data = event.data
          if data.from == nil or data.to == nil then
            args = { fname = data.from or data.to }
          else
            args = { old_name = data.from, new_name = data.to }
          end
          action.callback(args)
        end,
      })
    end

    vim.api.nvim_create_autocmd('User', {
      pattern = 'MiniFilesActionRename',
      group = au_group,
      desc = 'LSP Rename file',
      callback = function(event)
        if not Snacks then
          return
        end
        Snacks.rename.on_rename_file(event.data.from, event.data.to)
      end,
    })
  end,
}
