---@type LazySpec
return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      setup = {
        tailwindcss = function()
          LazyVim.lsp.on_attach(function(client)
            client.server_capabilities.completionProvider.triggerCharacters =
              { '"', "'", "`", ".", "(", "[", "!", "/", ":" }
          end, "sp_tailwind")
        end,
      },
      -- make sure mason installs the server
      servers = {
        vtsls = {
          settings = {
            vtsls = { experimental = { completion = { enableServerSideFuzzyMatch = false } } },
          },
        },
        eslint = { keys = { { "<leader>le", "<cmd>EslintFixAll<cr>", desc = "Eslint Fix" } } },
        pyright = false,
        basedpyright = {
          settings = {
            basedpyright = {
              analysis = {
                ---@type "standard"|"basic"
                typeCheckingMode = "standard",
                autoImportCompletions = true,
                diagnosticSeverityOverrides = {
                  reportUnusedImport = "information",
                  reportUnusedFunction = "information",
                  reportUnusedVariable = "information",
                  reportGeneralTypeIssues = "none",
                  reportOptionalMemberAccess = "warning",
                  reportOptionalSubscript = "none",
                  reportIgnoreCommentWithoutRule = "warning",
                  reportUnreachable = "error",
                  reportPrivateLocalImportUsage = "error",
                  reportImplicitRelativeImport = "error",
                  reportInvalidCast = "error",
                  -- reportPrivateImportUsage = "none",
                },
              },
            },
          },
        },
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = function()
      local keys = require("lazyvim.plugins.lsp.keymaps").get()
      keys[#keys + 1] = { "gl", vim.diagnostic.open_float, desc = "Open diagnostic" }
      --{{{  disable a keymap
      keys[#keys + 1] = { "<C-k>", mode = "i", false }
      -- I'm used to <leader>l being my "lsp" prefix. Can't change my musle memory now
      keys[#keys + 1] = { "<leader>cl", false }
      keys[#keys + 1] = { "<leader>ca", false }
      keys[#keys + 1] = { "<leader>cc", false }
      keys[#keys + 1] = { "<leader>cC", false }
      keys[#keys + 1] = { "<leader>cR", false }
      keys[#keys + 1] = { "<leader>cr", false }
      keys[#keys + 1] = { "<leader>cA", false }
      keys[#keys + 1] = { "<leader>co", false }
      keys[#keys + 1] = { "<leader>cM", false }
      keys[#keys + 1] = { "<leader>cu", false }
      keys[#keys + 1] = { "<leader>cD", false }
      keys[#keys + 1] = { "<leader>cV", false }
      keys[#keys + 1] = { "<leader>cp", false }
      keys[#keys + 1] = { "<leader>cP", false }
      keys[#keys + 1] = { "<leader>ch", false }
      keys[#keys + 1] = { "<leader>ss", false }
      keys[#keys + 1] = { "<leader>sS", false }
      --}}}
      keys[#keys + 1] = {
        "<C-h>",
        function()
          return vim.lsp.buf.signature_help()
        end,
        mode = "i",
        desc = "Signature Help",
        has = "signatureHelp",
      }
      keys[#keys + 1] = {
        "<leader>ll",
        function()
          Snacks.picker.lsp_config()
        end,
        desc = "Lsp Info",
      }
      keys[#keys + 1] =
        { "<leader>la", vim.lsp.buf.code_action, desc = "Code Action", mode = { "n", "v" }, has = "codeAction" }
      keys[#keys + 1] =
        { "<leader>lc", vim.lsp.codelens.run, desc = "Run Codelens", mode = { "n", "v" }, has = "codeLens" }
      keys[#keys + 1] = {
        "<leader>lC",
        vim.lsp.codelens.refresh,
        desc = "Refresh & Display Codelens",
        mode = { "n" },
        has = "codeLens",
      }
      keys[#keys + 1] = {
        "<leader>lR",
        function()
          Snacks.rename.rename_file()
        end,
        desc = "Rename File",
        mode = { "n" },
        has = { "workspace/didRenameFiles", "workspace/willRenameFiles" },
      }
      keys[#keys + 1] = { "<leader>lr", vim.lsp.buf.rename, desc = "Rename", has = "rename" }
      keys[#keys + 1] = {
        "<leader>lw",
        function()
          Snacks.picker.lsp_symbols({ filter = LazyVim.config.kind_filter })
        end,
        desc = "LSP Symbols",
        has = "documentSymbol",
      }
      keys[#keys + 1] = {
        "<leader>lW",
        function()
          Snacks.picker.lsp_workspace_symbols({ filter = LazyVim.config.kind_filter })
        end,
        desc = "LSP Workspace Symbols",
        has = "workspace/symbols",
      }
      keys[#keys + 1] = { "<leader>lA", LazyVim.lsp.action.source, desc = "Source Action", has = "codeAction" }
      keys[#keys + 1] = { "<leader>lo", LazyVim.lsp.action["source.organizeImports"], desc = "Organize Imports" }
      keys[#keys + 1] = {
        "<leader>lM",
        LazyVim.lsp.action["source.addMissingImports.ts"],
        desc = "Add missing imports",
        ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
      }
      keys[#keys + 1] = {
        "<leader>lu",
        LazyVim.lsp.action["source.removeUnused.ts"],
        desc = "Remove unused imports",
        ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
      }
      keys[#keys + 1] = {
        "<leader>lF",
        LazyVim.lsp.action["source.fixAll.ts"],
        desc = "Fix all diagnostics",
        ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
      }
      keys[#keys + 1] = {
        "<leader>lV",
        function()
          LazyVim.lsp.execute({ command = "typescript.selectTypeScriptVersion" })
        end,
        desc = "Select TS workspace version",
        ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
      }
      keys[#keys + 1] = {
        "<leader>lp",
        ft = "markdown",
        "<cmd>MarkdownPreviewToggle<cr>",
        desc = "Markdown Preview",
      }
      keys[#keys + 1] = {
        "<leader>lh",
        "<cmd>ClangdSwitchSourceHeader<cr>",
        desc = "Switch Source/Header (C/C++)",
        ft = { "c", "cpp" },
      }
      keys[#keys + 1] = {
        "<leader>cp",
        function()
          local params = vim.lsp.util.make_position_params()
          LazyVim.lsp.execute({
            command = "manipulatePipes:serverid",
            arguments = { "toPipe", params.textDocument.uri, params.position.line, params.position.character },
          })
        end,
        ft = { "heex", "elixir" },
        desc = "To Pipe",
      }
      keys[#keys + 1] = {
        "<leader>cP",
        function()
          local params = vim.lsp.util.make_position_params()
          LazyVim.lsp.execute({
            command = "manipulatePipes:serverid",
            arguments = { "fromPipe", params.textDocument.uri, params.position.line, params.position.character },
          })
        end,
        ft = { "heex", "elixir" },
        desc = "From Pipe",
      }
    end,
  },
  {
    "folke/trouble.nvim",
    optional = true,
    keys = {
      { "<leader>cs", false },
      { "<leader>cS", false },
      { "<leader>ls", "<cmd>Trouble symbols toggle<cr>", desc = "Symbols (Trouble)" },
      { "<leader>lS", "<cmd>Trouble lsp toggle<cr>", desc = "LSP references/definitions/... (Trouble)" },
    },
  },
  {
    "mfussenegger/nvim-lint",
    opts = { linters_by_ft = { go = { "golangcilint" } } },
  },
  {
    "linux-cultist/venv-selector.nvim",
    optional = true,
    cmd = "VenvSelect",
    keys = {
      { "<leader>cv", false },
      { "<leader>lv", "<cmd>VenvSelect<cr>", desc = "Select VirtualEnv", ft = "python" },
    },
  },
}
