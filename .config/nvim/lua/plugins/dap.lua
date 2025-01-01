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
    'Weissle/persistent-breakpoints.nvim',
    'leoluz/nvim-dap-go',
    -- 'mxsdev/nvim-dap-vscode-js',
    -- {
    --   'microsoft/vscode-js-debug',
    --   version = false,
    --   build = 'npm install --legacy-peer-deps && npx gulp vsDebugServerBundle && mv dist out',
    -- },
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
    local dap, dapui = require 'dap', require 'dapui'
    local dap_icons = {
      Stopped = { '󰁕 ', 'DiagnosticWarn', 'DapStoppedLine' },
      Breakpoint = ' ',
      BreakpointCondition = ' ',
      BreakpointRejected = { ' ', 'DiagnosticError' },
      LogPoint = '.>',
    }
    vim.api.nvim_set_hl(0, 'DapStoppedLine', { default = true, link = 'Visual' })

    for name, sign in pairs(dap_icons) do
      sign = type(sign) == 'table' and sign or { sign }
      vim.fn.sign_define(
        'Dap' .. name,
        { text = sign[1], texthl = sign[2] or 'DiagnosticInfo', linehl = sign[3], numhl = sign[3] }
      )
    end
    require('persistent-breakpoints').setup({
      load_breakpoints_event = { 'BufReadPost' },
    })
    ---@diagnostic disable-next-line: missing-fields
    -- require('dap-vscode-js').setup({
    --   debugger_path = vim.fn.stdpath 'data' .. '/lazy/vscode-js-debug',
    --   adapters = { 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost' }, -- which adapters to register in nvim-dap
    -- })
    dap.adapters.bashdb = {
      type = 'executable',
      command = vim.fn.stdpath 'data' .. '/mason/packages/bash-debug-adapter/bash-debug-adapter',
      name = 'bashdb',
    }
    dap.adapters.mix_task = {
      type = 'executable',
      command = require('mason-registry').get_package('elixir-ls'):get_install_path() .. '/debug_adapter.sh', -- debugger.bat for windows
      args = {},
    }
    -- setup dap config by VsCode launch.json file
    local vscode = require 'dap.ext.vscode'
    local json = require 'plenary.json'
    vscode.json_decode = function(str)
      return vim.json.decode(json.json_strip_comments(str, { whitespace = true }))
    end
    if not dap.adapters['pwa-node'] then
      require('dap').adapters['pwa-node'] = {
        type = 'server',
        host = 'localhost',
        port = '${port}',
        executable = {
          command = require('mason-registry').get_package('js-debug-adapter'):get_install_path() .. '/js-debug-adapter',
          args = {
            --   require('mason-registry').get_package('js-debug-adapter'):get_install_path()
            --     .. '/js-debug/src/dapDebugServer.js',
            '${port}',
          },
        },
      }
    end
    if not dap.adapters['node'] then
      dap.adapters['node'] = function(cb, config)
        if config.type == 'node' then
          config.type = 'pwa-node'
        end
        local nativeAdapter = dap.adapters['pwa-node']
        if type(nativeAdapter) == 'function' then
          nativeAdapter(cb, config)
        else
          cb(nativeAdapter)
        end
      end
    end

    local js_filetypes = { 'typescript', 'javascript', 'typescriptreact', 'javascriptreact' }

    vscode.type_to_filetypes['node'] = js_filetypes
    vscode.type_to_filetypes['pwa-node'] = js_filetypes

    for _, language in ipairs(js_filetypes) do
      if not dap.configurations[language] then
        dap.configurations[language] = {
          {
            type = 'pwa-node',
            request = 'launch',
            name = 'Launch file',
            program = '${file}',
            cwd = '${workspaceFolder}',
          },
          {
            type = 'pwa-node',
            request = 'attach',
            name = 'Attach',
            processId = require('dap.utils').pick_process,
            cwd = '${workspaceFolder}',
          },
        }
      end
    end

    dap.adapters.php = {
      type = 'executable',
      command = require('mason-registry').get_package('php-debug-adapter'):get_install_path() .. 'php-debug-adapter',
      -- command = 'node',
      -- args = { require('mason-registry').get_package('php-debug-adapter'):get_install_path() .. '/out/phpDebug.js' }
    }

    dap.configurations.php = {
      {
        type = 'php',
        request = 'launch',
        name = 'Listen for Xdebug',
        port = 9000,
      },
    }

    dap.configurations.elixir = {
      {
        type = 'mix_task',
        name = 'mix (Default task)',
        request = 'launch',
        projectDir = '${workspaceRoot}',
      },
      {
        type = 'mix_task',
        name = 'mix test',
        task = 'test',
        taskArgs = { '--trace' },
        request = 'launch',
        startApps = true, -- for Phoenix projects
        projectDir = '${workspaceFolder}',
        requireFiles = {
          'test/**/test_helper.exs',
          'test/**/*_test.exs',
        },
      },
    }
    require('dap-go').setup()
    dapui.setup()
    require('nvim-dap-virtual-text').setup({})

    dap.listeners.before.attach.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.launch.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated.dapui_config = function()
      dapui.close()
    end
    dap.listeners.before.event_exited.dapui_config = function()
      dapui.close()
    end
  end,
}
