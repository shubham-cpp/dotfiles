return {
  'akinsho/nvim-toggleterm.lua',
  version = '*',
  opts = function(_, opts)
    opts.open_mapping = [[<c-\>]]
    return opts
  end,
}
