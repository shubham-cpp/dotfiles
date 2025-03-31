---@param config {type?:string, args?:string[]|fun():string[]?}
local function get_args(config)
  local args = type(config.args) == "function" and (config.args() or {}) or config.args or {} --[[@as string[] | string ]]
  local args_str = type(args) == "table" and table.concat(args, " ") or args --[[@as string]]

  config = vim.deepcopy(config)
  ---@cast args string[]
  config.args = function()
    local new_args = vim.fn.expand(vim.fn.input("Run with args: ", args_str)) --[[@as string]]
    if config.type and config.type == "java" then
      ---@diagnostic disable-next-line: return-type-mismatch
      return new_args
    end
    return require("dap.utils").splitstr(new_args)
  end
  return config
end
---@type LazySpec
return {
  {
    "mason.nvim",
    optional = true,
    opts = { ensure_installed = { "debugpy", "delve", "bash-debug-adapter", "js-debug-adapter" } },
  },
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "nvim-neotest/nvim-nio",
      "Weissle/persistent-breakpoints.nvim",
      "leoluz/nvim-dap-go",
      -- virtual text for the debugger
      {
        "theHamsta/nvim-dap-virtual-text",
        opts = {},
      },
      {
        "leoluz/nvim-dap-go",
        opts = {},
      },
    },
    keys = function()
      local dap, dapui, pb = require "dap", require "dapui", require "persistent-breakpoints.api"
      return {
        -- { "<leader>dB", function() dap.set_breakpoint(vim.fn.input('Breakpoint condition: ')) end, desc = "Breakpoint Condition" },
        -- { "<leader>db", dap.toggle_breakpoint, desc = "Toggle Breakpoint" },
        { "<leader>db", pb.toggle_breakpoint, desc = "Toggle Breakpoint" },
        {
          "<leader>dB",
          function() pb.set_breakpoint(vim.fn.input "Breakpoint condition: ") end,
          desc = "Breakpoint Condition",
        },
        { "<leader>dC", dap.run_to_cursor, desc = "Run to Cursor" },
        { "<leader>dc", dap.continue, desc = "Run/Continue" },
        { "<leader>dT", pb.clear_all_breakpoints, desc = "Clear All Breakpoints" },
        { "<leader>du", dapui.toggle, desc = "DapUI" },
        { "<leader>de", dapui.eval, desc = "DapUI Eval", mode = { "v", "n" } },

        { "<F8>", dap.continue, desc = "Run/Continue" },
        { "<F9>", dap.step_into, desc = "Step Into" },
        { "<F10>", dap.step_out, desc = "Step Out" },
        { "<F11>", dap.step_over, desc = "Step Over" },

        { "<leader>di", dap.step_into, desc = "Step Into" },
        { "<leader>do", dap.step_out, desc = "Step Out" },
        { "<leader>dO", dap.step_over, desc = "Step Over" },
        { "<leader>dt", dap.terminate, desc = "Terminate" },
        { "<leader>dp", dap.pause, desc = "Pause" },
        { "<leader>dj", dap.down, desc = "Down" },
        { "<leader>dk", dap.up, desc = "Up" },
      }
    end,
    config = function()
      local dap = require "dap"
      local dapui = require "dapui"

      vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })
      local dap_icons = {
        Stopped = { "󰁕 ", "DiagnosticWarn", "DapStoppedLine" },
        Breakpoint = " ",
        BreakpointCondition = " ",
        BreakpointRejected = { " ", "DiagnosticError" },
        LogPoint = ".>",
      }
      for name, sign in pairs(dap_icons) do
        sign = type(sign) == "table" and sign or { sign }
        vim.fn.sign_define(
          "Dap" .. name,
          { text = sign[1], texthl = sign[2] or "DiagnosticInfo", linehl = sign[3], numhl = sign[3] }
        )
      end

      require("persistent-breakpoints").setup({
        load_breakpoints_event = { "BufReadPost" },
      })

      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open({}) end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close({}) end
      dap.listeners.before.event_exited["dapui_config"] = function() dapui.close({}) end

      dap.adapters.bashdb = {
        type = "executable",
        command = require("mason-registry").get_package("bash-debug-adapter"):get_install_path()
          .. "/bash-debug-adapter",
        name = "bashdb",
      }
      dap.adapters.mix_task = {
        type = "executable",
        command = require("mason-registry").get_package("elixir-ls"):get_install_path() .. "/debug_adapter.sh", -- debugger.bat for windows
        args = {},
      }
      -- setup dap config by VsCode launch.json file
      local vscode = require "dap.ext.vscode"
      local json = require "plenary.json"
      vscode.json_decode = function(str) return vim.json.decode(json.json_strip_comments(str)) end

      if not dap.adapters["pwa-node"] then
        require("dap").adapters["pwa-node"] = {
          type = "server",
          host = "localhost",
          port = "${port}",
          executable = {
            command = "node",
            -- 💀 Make sure to update this path to point to your installation
            args = {
              require("mason-registry").get_package("js-debug-adapter"):get_install_path()
                .. "/js-debug/src/dapDebugServer.js",
              "${port}",
            },
          },
        }
      end
      if not dap.adapters["node"] then
        dap.adapters["node"] = function(cb, config)
          if config.type == "node" then config.type = "pwa-node" end
          local nativeAdapter = dap.adapters["pwa-node"]
          if type(nativeAdapter) == "function" then
            nativeAdapter(cb, config)
          else
            cb(nativeAdapter)
          end
        end
      end

      local js_filetypes = { "typescript", "javascript", "typescriptreact", "javascriptreact" }

      vscode.type_to_filetypes["node"] = js_filetypes
      vscode.type_to_filetypes["pwa-node"] = js_filetypes

      for _, language in ipairs(js_filetypes) do
        if not dap.configurations[language] then
          dap.configurations[language] = {
            {
              type = "pwa-node",
              request = "launch",
              name = "Launch file",
              program = "${file}",
              cwd = "${workspaceFolder}",
            },
            {
              type = "pwa-node",
              request = "attach",
              name = "Attach",
              processId = require("dap.utils").pick_process,
              cwd = "${workspaceFolder}",
            },
          }
        end
      end
    end,
  },
}
