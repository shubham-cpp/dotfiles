local au_session = vim.api.nvim_create_augroup('sp_sessions', { clear = true })
local function get_session_name()
  local name = vim.fn.getcwd()
  local branch = vim.trim(vim.fn.system 'git branch --show-current')
  if vim.v.shell_error == 0 then
    return name .. branch
  else
    return name
  end
end
return {
  {
    'Shatur/neovim-session-manager',
    event = 'BufWinEnter',
    enabled = true,
    cmd = 'SessionManager',
    keys = {
      { '<leader>sl', '<cmd>SessionManager! load_session<cr>', desc = '[S]ession [L]oad' },
      { '<leader>sL', '<cmd>SessionManager! load_last_session<cr>', desc = '[S]ession [L]oad Last' },
      {
        '<leader>sc',
        '<cmd>SessionManager! load_current_dir_session<cr>',
        desc = '[S]ession Load Current Directory',
      },
    },
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- {
      --   'stevearc/dressing.nvim',
      --   opts = {
      --     select = {
      --       backend = { 'fzf_lua', 'telescope', 'builtin', 'nui' },
      --     },
      --   },
      -- },
    },
    config = function()
      local session_manager = require 'session_manager'
      session_manager.setup({
        autoload_mode = require('session_manager.config').AutoloadMode.CurrentDir, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
      })
    end,
  },
  {
    'stevearc/resession.nvim',
    enabled = false,
    lazy = true,
    init = function()
      vim.api.nvim_create_autocmd('UIEnter', {
        callback = function()
          if vim.fn.argc(-1) == 0 then
            require('resession').load(get_session_name(), { dir = 'dirsession', silence_errors = true })
          end
        end,
        group = au_session,
      })
      vim.api.nvim_create_autocmd('VimLeavePre', {
        callback = function()
          require('resession').save(get_session_name(), { dir = 'dirsession', notify = false, silence_errors = true })
        end,
        group = au_session,
      })
      vim.api.nvim_create_autocmd({ 'VimLeave' }, {
        callback = function()
          vim.cmd 'sleep 50m'
        end,
        group = au_session,
      })
    end,
    config = function()
      local resession = require 'resession'
      resession.setup({
        -- Save and restore these options
        options = {
          'binary',
          'bufhidden',
          'buflisted',
          'cmdheight',
          'diff',
          'filetype',
          'modifiable',
          'previewwindow',
          'readonly',
          'scrollbind',
          'winfixheight',
          'winfixwidth',
        },
      })
      -- vim.keymap.set('n', '<leader>ss', resession.save, { desc = 'Save' })
      -- vim.keymap.set('n', '<leader>sl', resession.load, { desc = 'Load' })
      -- vim.keymap.set('n', '<leader>sd', resession.delete, { desc = 'Delete' })
    end,
  },
}
