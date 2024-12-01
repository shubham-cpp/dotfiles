local au_group = vim.api.nvim_create_augroup('sp_sessions', { clear = true })
---@type LazySpec
return {
  {
    'Shatur/neovim-session-manager',
    event = 'BufWinEnter',
    enabled = false,
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
        -- Possible values: Disabled, CurrentDir, LastSession
        autoload_mode = require('session_manager.config').AutoloadMode.CurrentDir,
      })
    end,
  },
  {
    'stevearc/resession.nvim',
    enabled = false,
    lazy = true,
    init = function()
      local function get_session_name()
        local name = vim.fn.getcwd()
        local branch = vim.trim(vim.fn.system 'git branch --show-current')
        if vim.v.shell_error == 0 then
          return name .. branch
        else
          return name
        end
      end
      vim.api.nvim_create_autocmd('VimEnter', {
        desc = 'Load session on VimEnter',
        callback = function()
          -- Only load the session if nvim was started with no args
          if vim.fn.argc(-1) == 0 then
            vim.schedule(function()
              require('resession').load(get_session_name(), { dir = 'dirsession', silence_errors = true })
            end)
          end
        end,
        nested = true,
      })
      vim.api.nvim_create_autocmd('VimLeavePre', {
        desc = 'Save session on VimLeavePre',
        callback = function()
          require('resession').save(get_session_name(), { dir = 'dirsession', notify = false })
        end,
      })
    end,
    opts = {},
    config = function(_, opts)
      require('resession').setup(opts)
    end,
    keys = {
      { '<leader>ss', '<cmd>lua require("resession").save()<cr>', desc = '[S]ession [S]ave' },
      { '<leader>sd', '<cmd>lua require("resession").delete()<cr>', desc = '[S]ession [D]elete' },
      {
        '<leader>sl',
        function()
          vim.ui.select(
            require('resession').list({ dir = 'dirsession' }),
            { prompt = 'Select Session> ' },
            function(selected_session)
              if selected_session == nil or not selected_session then
                return
              end
              require('resession').load(selected_session, { dir = 'dirsession', silence_errors = true })
            end
          )
        end,
        desc = '[S]ession [L]ist',
      },
    },
  },
  {
    'rmagatti/auto-session',
    lazy = false,
    ---enables autocomplete for opts
    ---@module "auto-session"
    ---@type AutoSession.Config
    opts = {
      suppressed_dirs = { '~/', '~/Projects', '~/Downloads', '/' },
      use_git_branch = true,
    },
    keys = {
      { '<leader>s', '', desc = '+[S]ession' },
      { '<leader>ss', '<cmd>SessionSave<CR>', desc = '[S]ave' },
      { '<leader>sl', '<cmd>AutoSession search<cr>', desc = '[L]ist' },
      { '<leader>sd', '<cmd>AutoSession delete<cr>', desc = '[D]elte' },
    },
  },
}
