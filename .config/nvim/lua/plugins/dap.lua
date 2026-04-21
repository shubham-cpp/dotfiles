return {
  {
    "mfussenegger/nvim-dap",
    dependencies = { "rcarriga/nvim-dap-ui" },
    keys = function() 
      return {
        { "<F5>", function() require("dap").continue() end, desc = "DAP: Continue" },
        { "<F9>", function() require("dap").toggle_breakpoint() end, desc = "DAP: Toggle breakpoint" },
        { "<F17>", function() require("dap").terminate() end, desc = "DAP: Terminate" },
        { "<F29>", function() require("dap").restart_frame() end, desc = "DAP: Restart frame" },
        { "<F6>", function() require("dap").pause() end, desc = "DAP: Pause" },
        { "<F10>", function() require("dap").step_over() end, desc = "DAP: Step over" },
        { "<F11>", function() require("dap").step_into() end, desc = "DAP: Step into" },
        { "<F23>", function() require("dap").step_out() end, desc = "DAP: Step out" },
        { "<F21>", function()
          vim.ui.input({ prompt = "Breakpoint condition: " }, function(condition)
            if condition and condition ~= "" then
              require("dap").toggle_breakpoint(condition)
            end
          end)
        end, desc = "DAP: Conditional breakpoint" },
      }
    end,
  },
  {
    "rcarriga/nvim-dap-ui",
    dependencies = { "nvim-neotest/nvim-nio" },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      dapui.setup()
      dap.listeners.before.attach.dapui_config = function() dapui.open() end
      dap.listeners.before.launch.dapui_config = function() dapui.open() end
      dap.listeners.before.event_terminated.dapui_config = function() dapui.close() end
      dap.listeners.before.event_exited.dapui_config = function() dapui.close() end
    end,
  },
}
