return {
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'BufReadPost',
    enabled = false,
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
    'echasnovski/mini.indentscope',
    version = false,
    enabled = true,
    event = 'BufReadPost',
    config = function()
      require('mini.indentscope').setup({
        -- Which character to use for drawing scope indicator
        symbol = '╎',
      })
    end,
  },
}
