local au_group = vim.api.nvim_create_augroup('sp_mini', { clear = true })

local function get_clients(opts)
  local ret = {} ---@type vim.lsp.Client[]
  if vim.lsp.get_clients then
    ret = vim.lsp.get_clients(opts)
  else
    ---@diagnostic disable-next-line: deprecated
    ret = vim.lsp.get_active_clients(opts)
    if opts and opts.method then
      ---@param client vim.lsp.Client
      ret = vim.tbl_filter(function(client)
        return client.supports_method(opts.method, { bufnr = opts.bufnr })
      end, ret)
    end
  end
  return opts and opts.filter and vim.tbl_filter(opts.filter, ret) or ret
end

---@param from string
---@param to string
---@param rename? fun()
local function on_rename(from, to, rename)
  local changes = { files = { {
    oldUri = vim.uri_from_fname(from),
    newUri = vim.uri_from_fname(to),
  } } }

  local clients = get_clients()
  for _, client in ipairs(clients) do
    if client.supports_method 'workspace/willRenameFiles' then
      local resp = client.request_sync('workspace/willRenameFiles', changes, 1000, 0)
      if resp and resp.result ~= nil then
        vim.lsp.util.apply_workspace_edit(resp.result, client.offset_encoding)
      end
    end
  end

  if rename then
    rename()
  end

  for _, client in ipairs(clients) do
    if client.supports_method 'workspace/didRenameFiles' then
      client.notify('workspace/didRenameFiles', changes)
    end
  end
end

return {
  'echasnovski/mini.files',
  version = '*',
  enabled = false,
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
        on_rename(event.data.from, event.data.to)
      end,
    })
  end,
}
