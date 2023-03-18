return {
  'akinsho/nvim-toggleterm.lua',
  version = '*',
  config = function(_, opts)
    opts.open_mapping = [[<c-\>]]
    return opts
  end,
}
