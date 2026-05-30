return {
  {
    "mfussenegger/nvim-dap",
    dependencies = { "rcarriga/nvim-dap-ui" },
    lazy = false,
    keys = {
      { "<leader>dc", "<cmd>DapContinue<cr>", desc = "DAP: Continue" },
      { "<leader>db", "<cmd>DapToggleBreakpoint<cr>", desc = "DAP: Toggle breakpoint" },
      { "<leader>dC", "<cmd>DapClearBreakpoints<cr>", desc = "DAP: Clear breakpoints" },
      { "<leader>dt", "<cmd>DapTerminate<cr>", desc = "DAP: Terminate" },
      { "<leader>dd", "<cmd>DapDisconnect<cr>", desc = "DAP: Disconnect" },
      { "<leader>dr", "<cmd>DapRestartFrame<cr>", desc = "DAP: Restart frame" },
      { "<leader>dp", "<cmd>DapPause<cr>", desc = "DAP: Pause" },
      { "<leader>do", "<cmd>DapStepOver<cr>", desc = "DAP: Step over" },
      { "<leader>di", "<cmd>DapStepInto<cr>", desc = "DAP: Step into" },
      { "<leader>du", "<cmd>DapStepOut<cr>", desc = "DAP: Step out" },
      { "<leader>dR", "<cmd>DapToggleRepl<cr>", desc = "DAP: Toggle REPL" },
      { "<leader>dl", "<cmd>DapShowLog<cr>", desc = "DAP: Show log" },
    },
    config = function()
      local dap = require("dap")

      local function get_free_port()
        return math.random(30000, 60000)
      end

      local function executable(cmd, fallback)
        return function()
          local root = vim.fs.root(0, { "package.json", "tsconfig.json", "jsconfig.json", ".git" })
          if root then
            local local_cmd = root .. "/node_modules/.bin/" .. cmd
            if vim.fn.executable(local_cmd) == 1 then
              return local_cmd
            end
          end
          return vim.fn.executable(cmd) == 1 and cmd or fallback
        end
      end

      dap.adapters["pwa-node"] = function(callback)
        local port = get_free_port()
        callback({
          type = "server",
          host = "127.0.0.1",
          port = port,
          executable = {
            command = vim.fn.stdpath("data") .. "/mason/bin/js-debug-adapter",
            args = { tostring(port), "127.0.0.1" },
          },
        })
      end

      local js_node_configs = {
        {
          type = "pwa-node",
          request = "attach",
          name = "Attach to Node process",
          processId = require("dap.utils").pick_process,
          cwd = "${workspaceFolder}",
        },
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch current file with tsx",
          runtimeExecutable = executable("tsx"),
          runtimeArgs = { "${file}" },
          cwd = "${workspaceFolder}",
          console = "integratedTerminal",
        },
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch current file with node",
          program = "${file}",
          cwd = "${workspaceFolder}",
          console = "integratedTerminal",
        },
      }

      for _, ft in ipairs({
        "javascript",
        "javascriptreact",
        "javascript.jsx",
        "typescript",
        "typescriptreact",
        "typescript.tsx",
      }) do
        dap.configurations[ft] = js_node_configs
      end
    end,
  },
  {
    "rcarriga/nvim-dap-ui",
    dependencies = { "nvim-neotest/nvim-nio" },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      dapui.setup()
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
  },
}
