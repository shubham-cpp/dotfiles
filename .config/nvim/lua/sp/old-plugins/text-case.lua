return {
  {
    'johmsalas/text-case.nvim',
    enabled = false,
    config = function()
      require('textcase').setup({
        -- prefix = "ga",
      })
    end,
    keys = {
      'ga', -- Default invocation prefix
      { 'ga.', '<cmd>TextCaseOpenTelescope<CR>', mode = { 'n', 'x' }, desc = 'Telescope' },
    },
    -- cmd = {
    --   -- NOTE: The Subs command name can be customized via the option "substitude_command_name"
    --   'Subs',
    --   'TextCaseOpenTelescope',
    --   'TextCaseOpenTelescopeQuickChange',
    --   'TextCaseOpenTelescopeLSPChange',
    --   'TextCaseStartReplacingCommand',
    -- },
  },
}
