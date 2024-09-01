return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  cmd = 'FzfLua',
  enabled = true,
  config = function()
    require 'plugins.config.fzf'
  end,
  keys = {
    '<C-p>',
    '<leader>ff',
    '<leader>fr',
    '<leader>fs',
    '<leader>fc',
    '<leader>fS',

    { '<leader>fw', mode = { 'n', 'v' } },
    '<leader>fW',
    '<leader>fo',
    '<leader>fb',
    '<leader>fz',
    '<leader>fk',
    '<leader>fh',
    '<leader>fD',
    '<leader>fd',

    '<leader>fn',

    '<leader>fg',
    '<leader>gt',
    '<leader>gS',
    '<leader>gb',
    '<leader>gc',
    '<leader>gC',

    { '<C-x><C-k>', mode = 'i', desc = 'Complete bline' },
    { '<C-x><C-l>', mode = 'i', desc = 'Complete line' },
    { '<C-x><C-f>', mode = 'i', desc = 'Complete path' },
  },
}
