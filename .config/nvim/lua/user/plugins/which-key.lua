---@type LazySpec
return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  opts_extend = { 'spec' },
  opts = {
    ---@type 'modern'|'helix'|'classic'
    preset = 'helix',
    spec = {
      {
        mode = { 'n', 'v' },
        { '<leader>r', group = 'refactoring' },
        { '<leader>t', group = 'tests' },
        { '<leader>l', group = 'lsp' },
        { '<leader>u', group = 'toggle' },
        { '<leader>m', group = 'multi-cursor' },
        { '<leader>g', group = 'git' },
        { '<leader>n', group = 'neogen' },
        { '[', group = 'prev' },
        { ']', group = 'next' },
        { 'g', group = 'goto' },
        { 'sa', group = 'surround' },
        { 'z', group = 'fold' },
        -- better descriptions
        { 'gx', desc = 'Open with system app' },
      },
      {
        mode = { 'n' },
        { '<leader>S', group = 'session' },
        { '<leader>b', group = 'buffer' },
        { '<leader>o', group = 'open' },
      },
    },
  },
}
