return {
  'akinsho/toggleterm.nvim',
  version = '*',
  opts = {
    open_mapping = [[<c-\>]],
    winbar = { enabled = true },
    -- shade_terminals = false,
    highlights = {
      -- highlights which map to a highlight group name and a table of it's values
      -- NOTE: this is only a subset of values, any group placed here will be set for the terminal window split
      Normal = {
        guibg = '#32343d',
      },
      NormalFloat = {
        link = 'Normal',
      },
    },
  },
}
