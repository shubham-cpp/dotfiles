return {
  {
    url = "mfussenegger/nvim-dap",
    config = function()
      local dap = require("dap")

      vim.keymap.set("n", "<F5>", dap.continue, { desc = "DAP: Continue" })
      vim.keymap.set("n", "<F9>", dap.toggle_breakpoint, { desc = "DAP: Toggle breakpoint" })
      vim.keymap.set("n", "<F17>", dap.terminate, { desc = "DAP: Terminate" })
      vim.keymap.set("n", "<F29>", dap.restart_frame, { desc = "DAP: Restart frame" })
      vim.keymap.set("n", "<F6>", dap.pause, { desc = "DAP: Pause" })
      vim.keymap.set("n", "<F10>", dap.step_over, { desc = "DAP: Step over" })
      vim.keymap.set("n", "<F11>", dap.step_into, { desc = "DAP: Step into" })
      vim.keymap.set("n", "<F23>", dap.step_out, { desc = "DAP: Step out" })
      vim.keymap.set("n", "<F21>", function()
        vim.ui.input({ prompt = "Breakpoint condition: " }, function(condition)
          if condition and condition ~= "" then
            dap.toggle_breakpoint(condition)
          end
        end)
      end, { desc = "DAP: Conditional breakpoint" })
    end,
  },

  {
    url = "rcarriga/nvim-dap-ui",
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

  {
    url = "nvim-neotest/nvim-nio",
  },
}
