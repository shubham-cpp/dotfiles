---@type LazySpec
return {
  'echasnovski/mini.comment',
  version = '*',
  enabled = true,
  keys = {
    { 'gc', mode = { 'n', 'x', 'o' } },
    { 'gcc' },
    {
      '<leader>/',
      function()
        return require('mini.comment').operator() .. '_'
      end,
      desc = 'Toggle comment',
      expr = true,
      mode = 'n',
    },
    {
      '<leader>/',
      function()
        return require('mini.comment').operator()
      end,
      desc = 'Toggle comment',
      expr = true,
      mode = 'x',
    },
  },
  dependencies = {
    {
      'JoosepAlviste/nvim-ts-context-commentstring',
      -- config = function()
      --   -- vim.g.skip_ts_context_commentstring_module = true
      --   require('ts_context_commentstring').setup({
      --     enable_autocmd = false,
      --   })
      -- end,
    },
  },
  config = function()
    require('ts_context_commentstring').setup({
      enable_autocmd = false,
    })
    require('mini.comment').setup({
      options = {
        custom_commentstring = function()
          return require('ts_context_commentstring').calculate_commentstring() or vim.bo.commentstring
        end,
      },
    })
  end,
}
