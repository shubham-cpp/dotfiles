---@type LazySpec
return {
  'folke/snacks.nvim',
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = true },
    notifier = { enabled = true },
    quickfile = { enabled = true },
    statuscolumn = {
      enabled = true,
      folds = {
        open = true, -- show open fold icons
      },
    },
    words = { enabled = true },
    scope = { enabled = true },
    indent = {
      enabled = true,
      indent = {
        only_current = true,
        chunk = {
          -- when enabled, scopes will be rendered as chunks, except for the
          -- top-level scope which will be rendered as a scope.
          enabled = true,
        },
      },
      chunk = {
        -- when enabled, scopes will be rendered as chunks, except for the
        -- top-level scope which will be rendered as a scope.
        enabled = true,
      },
    },
  },
  keys = {
    {
      '<leader>gg',
      function()
        Snacks.lazygit()
      end,
      desc = 'Lazygit',
    },
    {
      '<leader>gL',
      function()
        Snacks.git.blame_line()
      end,
      desc = 'Git Blame Line',
    },
    {
      '<leader>go',
      function()
        Snacks.gitbrowse.open()
      end,
      desc = 'Git Open Line',
    },
    {
      ']]',
      function()
        Snacks.words.jump(vim.v.count1)
      end,
      desc = 'Next Reference',
      mode = { 'n', 't' },
    },
    {
      '[[',
      function()
        Snacks.words.jump(-vim.v.count1)
      end,
      desc = 'Prev Reference',
      mode = { 'n', 't' },
    },

    -- { "<leader>lR", function() Snacks.rename.rename_file() end, desc = "Rename File" },
  },
  init = function()
    vim.api.nvim_create_user_command('NotificationHistory', function()
      if not _G.Snacks then
        return
      end
      Snacks.notifier.show_history()
    end, { desc = 'Show Notification History' })
  end,
}
