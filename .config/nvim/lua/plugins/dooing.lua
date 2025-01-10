---@type LazySpec
return {
  'atiladefreitas/dooing',
  keys = { '<leader>td' },
  enabled = false,
  cmd = { 'Dooing' },
  opts = {
    -- Keymaps
    keymaps = {
      toggle_window = '<leader>td',
    },
  },
  config = function(_, opts)
    require('dooing').setup(opts)
  end,
}
