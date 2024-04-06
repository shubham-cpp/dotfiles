return {
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'BufReadPost',
    main = 'ibl',
    enabled = true,
    opts = {
      -- indent = { char = "▏" },
      scope = { show_exact_scope = true },
      exclude = {
        buftypes = {
          'nofile',
          'terminal',
        },
        filetypes = {
          'help',
          'startify',
          'aerial',
          'alpha',
          'dashboard',
          'lazy',
          'neogitstatus',
          'NvimTree',
          'neo-tree',
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
