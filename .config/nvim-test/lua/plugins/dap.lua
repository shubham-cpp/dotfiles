---@type LazySpec
return {
  'mfussenegger/nvim-dap',
  dependencies = {
    {
      'theHamsta/nvim-dap-virtual-text',
      opts = { virt_text_win_col = 80 },
    },
    'rcarriga/nvim-dap-ui',
    'nvim-neotest/nvim-nio',
    'mxsdev/nvim-dap-vscode-js',
    'Weissle/persistent-breakpoints.nvim',
    {
      'microsoft/vscode-js-debug',
      version = false,
      build = 'npm install --legacy-peer-deps && npx gulp vsDebugServerBundle && mv dist out',
    },
  },
  keys = function()
    local dap, dapui, pb = require 'dap', require 'dapui', require 'persistent-breakpoints.api'

    return {
      { '<leader>d', '', desc = '+debug', mode = { 'n', 'v' } },
      { '<leader>dO', dap.step_out, desc = 'Step Out' },
      { '<leader>do', dap.step_over, desc = 'Step Over' },
      { '<leader>di', dap.step_into, desc = 'Step Into' },
      { '<leader>dc', dap.continue, desc = 'Continue' },
      { '<leader>dp', dap.pause, desc = 'Pause' },
      { '<leader>dr', dap.repl.toggle, desc = 'Toggle REPL' },
      { '<leader>ds', dap.session, desc = 'Session' },
      { '<leader>dt', dap.terminate, desc = 'Terminate' },
      { '<leader>dT', pb.clear_all_breakpoints, desc = 'Clear All Breakpoints' },
      {
        '<leader>dw',
        function()
          require('dap.ui.widgets').hover()
        end,
        desc = 'Widgets',
      },
      { '<leader>db', pb.toggle_breakpoint, desc = 'Toggle Breakpoint' },
      {
        '<leader>dB',
        function()
          pb.set_breakpoint(vim.fn.input 'Breakpoint condition: ')
        end,
        desc = 'Breakpoint Condition',
      },

      { '<leader>du', dapui.toggle, desc = 'DapUI' },
      { '<leader>de', dapui.eval, desc = 'DapUI Eval', mode = { 'v', 'n' } },

      { '<F8>', dap.continue, desc = 'Continue' },
      { '<F10>', dap.step_over, desc = 'Step Over' },
      { '<F11>', dap.step_into, desc = 'Step Into' },
      { '<S-F11>', dap.step_over, desc = 'Step Over' },
    }
  end,
  config = function()
    require 'plugins.config.dap'
  end,
}
