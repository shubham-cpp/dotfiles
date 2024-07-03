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
require('dap-vscode-js').setup({
  debugger_path = vim.fn.stdpath 'data' .. '/lazy/vscode-js-debug',
  adapters = { 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost' }, -- which adapters to register in nvim-dap
})
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
