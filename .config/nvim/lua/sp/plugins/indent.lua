return {
  {
    'Darazaki/indent-o-matic',
    event = 'InsertEnter',
    config = true,
  },
  {
    'lukas-reineke/indent-blankline.nvim',
    version = "2.*",
    event = { 'BufReadPost', 'BufNewFile' },
    opts = {
      -- char = "▏",
      -- char = '│',
      filetype_exclude = { 'help', 'alpha', 'dashboard', 'neo-tree', 'Trouble', 'lazy' },
      show_current_context = true,
      show_current_context_start = true,
    },
  },
}
