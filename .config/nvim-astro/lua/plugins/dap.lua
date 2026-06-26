---@type LazySpec
return {
  "mfussenegger/nvim-dap",
  optional = true,
  opts = function()
    local dap = require "dap"
    local adapterType = "node"
    local pwaType = "pwa-" .. "node"
    if not dap.adapters[pwaType] then
      dap.adapters[pwaType] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "js-debug-adapter",
          args = { "${port}" },
        },
      }
    end
    local function executable(cmd, fallback)
      return function()
        local root = vim.fs.root(0, { "package.json", "tsconfig.json", "jsconfig.json", ".git" })
        if root then
          local local_cmd = root .. "/node_modules/.bin/" .. cmd
          if vim.fn.executable(local_cmd) == 1 then return local_cmd end
        end
        return vim.fn.executable(cmd) == 1 and cmd or fallback
      end
    end

    if not dap.adapters[adapterType] then
      dap.adapters[adapterType] = function(cb, config)
        local nativeAdapter = dap.adapters[pwaType]

        config.type = pwaType

        if type(nativeAdapter) == "function" then
          nativeAdapter(cb, config)
        else
          cb(nativeAdapter)
        end
      end
    end
    local js_filetypes = { "typescript", "javascript", "typescriptreact", "javascriptreact" }

    local vscode = require "dap.ext.vscode"
    vscode.type_to_filetypes["node"] = js_filetypes
    vscode.type_to_filetypes["pwa-node"] = js_filetypes

    for _, language in ipairs(js_filetypes) do
      if not dap.configurations[language] then
        dap.configurations[language] = {
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch file(Node)",
            program = "${file}",
            cwd = "${workspaceFolder}",
            runtimeExecutable = executable "node",
            skipFiles = {
              "<node_internals>/**",
              "node_modules/**",
            },
            resolveSourceMapLocations = {
              "${workspaceFolder}/**",
              "!**/node_modules/**",
            },
          },
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch file(tsx)",
            program = "${file}",
            cwd = "${workspaceFolder}",
            sourceMaps = true,
            runtimeExecutable = executable("tsx", "node"),
            skipFiles = {
              "<node_internals>/**",
              "node_modules/**",
            },
            resolveSourceMapLocations = {
              "${workspaceFolder}/**",
              "!**/node_modules/**",
            },
          },
          {
            type = "pwa-node",
            request = "attach",
            name = "Attach",
            processId = require("dap.utils").pick_process,
            cwd = "${workspaceFolder}",
            sourceMaps = true,
            runtimeExecutable = executable "node",
            skipFiles = {
              "<node_internals>/**",
              "node_modules/**",
            },
            resolveSourceMapLocations = {
              "${workspaceFolder}/**",
              "!**/node_modules/**",
            },
          },
        }
      end
    end
  end,
}
