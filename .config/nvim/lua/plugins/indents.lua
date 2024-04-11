return {
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'BufReadPost',
    enabled = true,
    main = 'ibl',
    opts = {
      indent = { char = '▏' },
      scope = { show_start = false, show_end = false },
      exclude = {
        buftypes = { 'nofile', 'prompt', 'quickfix', 'terminal' },
        filetypes = {
          'aerial',
          'alpha',
          'dashboard',
          'help',
          'lazy',
          'mason',
          'neo-tree',
          'NvimTree',
          'neogitstatus',
          'notify',
          'startify',
          'toggleterm',
          'Trouble',
        },
      },
    },
  },
  {
    'nvimdev/indentmini.nvim',
    enabled = false,
    event = 'BufEnter',
    config = function()
      require('indentmini').setup()
    end,
  },
}
