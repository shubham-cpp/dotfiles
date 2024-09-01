---@type LazySpec
return {
  {
    'numToStr/Comment.nvim',
    enabled = false,
    keys = {
      { 'gc', mode = { 'x', 'n' }, desc = 'comment' },
      { 'gb', desc = 'Comment(block)' },
      { '<leader>/', '<cmd>lua require("Comment.api").toggle.linewise.current()<CR>', desc = 'Toggle Comment' },
      {
        '<leader>/',
        function()
          local esc = vim.api.nvim_replace_termcodes('<ESC>', true, false, true)
          vim.api.nvim_feedkeys(esc, 'nx', false)
          require('Comment.api').toggle.linewise(vim.fn.visualmode())
        end,
        mode = 'v',
        desc = 'Toggle Comment',
      },
      { '<A-/>', '"ayy"ap<cmd>lua require("Comment.api").toggle.linewise.current()<CR>', desc = 'Toggle Comment' },
    },
    dependencies = {
      {
        'JoosepAlviste/nvim-ts-context-commentstring',
        config = function()
          -- vim.g.skip_ts_context_commentstring_module = true
          require('ts_context_commentstring').setup({
            enable_autocmd = false,
          })
        end,
      },
    },
    config = function()
      require('Comment').setup({
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      })
    end,
  },
}
