return {
  'akinsho/toggleterm.nvim',
  keys = { '<C-\\>' },
  opts = function(_, opts)
    opts.open_mapping = [[<C-\>]]
    opts.on_open = function()
      local local_opts = { buffer = 0 }
      vim.keymap.set('t', '<C-]>', [[<C-\><C-n>]], local_opts)
      -- vim.keymap.set("t", "<C-w>", [[<C-\><C-n><C-w>]], opts)
    end
    return opts
  end,
}
