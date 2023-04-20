return {
  'Shatur/neovim-session-manager',
  event = 'BufWinEnter',
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
    {
      'stevearc/dressing.nvim',
      opts = {
        select = {
          backend = { 'fzf_lua', 'telescope', 'builtin', 'nui' },
        },
      },
    },
  },
  opts = {
    autoload_mode = 'Disabled',
  },
}
