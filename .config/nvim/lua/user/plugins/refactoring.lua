return {
  'ThePrimeagen/refactoring.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-treesitter/nvim-treesitter',
  },
  cmd = 'Refactor',
  keys = {
    { '<leader>re', ':Refactor extract ', mode = 'v', desc = 'Extract' },
    { '<leader>rf', ':Refactor extract_to_file ', mode = 'v', desc = 'Extract to file' },
    { '<leader>rv', ':Refactor extract_var ', mode = 'v', desc = 'Extract var' },
    { '<leader>ri', ':Refactor inline_var', mode = { 'v', 'n' }, desc = 'Inline var' },
    { '<leader>rI', ':Refactor inline_func', desc = 'Inline func' },
    { '<leader>rb', ':Refactor extract_block', desc = 'Extract block' },
    { '<leader>rB', ':Refactor extract_block_to_file', desc = 'Extract block to file' },
  },
  opts = {
    show_success_message = true,
  },
}
