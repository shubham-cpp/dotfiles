return {
  {
    'numToStr/Comment.nvim',
    keys = {
      { 'gc', mode = { 'x', 'n' } },
      'gb',
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
      { '<A-/>', '"ayy"apgcc', desc = 'Toggle Comment' },
    },
    dependencies = {
      {
        'JoosepAlviste/nvim-ts-context-commentstring',
        config = function()
          vim.g.skip_ts_context_commentstring_module = true
          require('ts_context_commentstring').setup({
            enable_autocmd = false,
          })
        end,
      },
    },
    config = function()
      require('Comment').setup({
        ignore = '^$',
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      })
    end,
  },
  {
    'danymat/neogen',
    cmd = 'Neogen',
    keys = {
      { '<leader>nn', '<cmd>Neogen<cr>', desc = 'Default' },
      { '<leader>nt', '<cmd>Neogen type<cr>', desc = 'Type' },
      { '<leader>nf', '<cmd>Neogen file<cr>', desc = 'File' },
      { '<leader>nc', '<cmd>Neogen class<cr>', desc = 'Class' },
    },
    config = true,
  },
}
