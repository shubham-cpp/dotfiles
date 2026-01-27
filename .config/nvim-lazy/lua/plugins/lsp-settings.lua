---@type LazySpec
return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      -- setup = {
      --   tailwindcss = function()
      --     Snacks.util.lsp.on({ name = "tailwindcss" }, function(_, client)
      --       client.server_capabilities.completionProvider.triggerCharacters =
      --       { '"', "'", "`", ".", "(", "[", "!", "/", ":" }
      --     end)
      --   end,
      -- },
      -- make sure mason installs the server
      servers = {
        ols = {},
        lua_ls = { enabled = false },
        emmylua_ls = { enabled = true },
        vtsls = {
          settings = {
            vtsls = { experimental = { completion = { enableServerSideFuzzyMatch = false } } },
          },
        },
        eslint = { keys = { { "<leader>le", "<cmd>EslintFixAll<cr>", desc = "Eslint Fix" } } },
        pyright = { enabled = false },
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
    opts = {
      servers = {
        ["*"] = {
          keys = {
            { "gl",         vim.diagnostic.open_float, desc = "Open diagnostic" },
            --{{{  disable a keymap
            { "<C-k>",      mode = "i",                false },
            -- I'm used to <leader>l being my "lsp" prefix. Can't change my muscle memory now
            { "<leader>cl", false },
            { "<leader>ca", false },
            { "<leader>cc", false },
            { "<leader>cC", false },
            { "<leader>cR", false },
            { "<leader>cr", false },
            { "<leader>cA", false },
            { "<leader>co", false },
            { "<leader>cM", false },
            { "<leader>cu", false },
            { "<leader>cD", false },
            { "<leader>cV", false },
            { "<leader>cp", false },
            { "<leader>cP", false },
            { "<leader>ch", false },
            { "<leader>ss", false },
            { "<leader>sS", false },
            --}}}
            {
              "<C-h>",
              function()
                return vim.lsp.buf.signature_help()
              end,
              mode = "i",
              desc = "Signature Help",
              has = "signatureHelp",
            },
            { "<leader>la", vim.lsp.buf.code_action, desc = "Code Action",  mode = { "n", "v" }, has = "codeAction" },
            { "<leader>lc", vim.lsp.codelens.run,    desc = "Run Codelens", mode = { "n", "v" }, has = "codeLens" },
            {
              "<leader>lC",
              vim.lsp.codelens.refresh,
              desc = "Refresh & Display Codelens",
              mode = { "n" },
              has = "codeLens",
            },
            {
              "<leader>lR",
              function()
                Snacks.rename.rename_file()
              end,
              desc = "Rename File",
              mode = { "n" },
              has = { "workspace/didRenameFiles", "workspace/willRenameFiles" },
            },
            { "<leader>lr", vim.lsp.buf.rename,                           desc = "Rename",          has = "rename" },
            {
              "<leader>lw",
              function()
                local ok_fzf, fzf = pcall(require, "fzf-lua")
                local ok_snacks, picker = pcall(require, "snacks.picker")
                if ok_fzf then
                  fzf.lsp_document_symbols({})
                elseif ok_snacks then
                  picker.lsp_symbols({ filter = LazyVim.config.kind_filter })
                else
                  vim.lsp.buf.document_symbol()
                end
              end,
              desc = "LSP Symbols",
              has = "documentSymbol",
            },
            {
              "<leader>lW",
              function()
                local ok_fzf, fzf = pcall(require, "fzf-lua")
                local ok_snacks, picker = pcall(require, "snacks.picker")
                if ok_fzf then
                  fzf.lsp_workspace_symbols({ fzf_cli_args = "--nth 1..2" })
                elseif ok_snacks then
                  picker.lsp_workspace_symbols({ filter = LazyVim.config.kind_filter })
                else
                  vim.lsp.buf.workspace_symbol()
                end
              end,
              desc = "LSP Workspace Symbols",
              has = "workspace/symbols",
            },
            {
              "<leader>lA",
              LazyVim.lsp.action.source,
              desc = "Source Action",
              has = "codeAction",
            },
            { "<leader>lo", LazyVim.lsp.action["source.organizeImports"], desc = "Organize Imports" },
            {
              "<leader>lM",
              LazyVim.lsp.action["source.addMissingImports.ts"],
              desc = "Add missing imports",
              ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
            },
            {
              "<leader>lu",
              LazyVim.lsp.action["source.removeUnused.ts"],
              desc = "Remove unused imports",
              ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
            },
            {
              "<leader>lF",
              LazyVim.lsp.action["source.fixAll.ts"],
              desc = "Fix all diagnostics",
              ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
            },
            {
              "<leader>lV",
              function()
                LazyVim.lsp.execute({ command = "typescript.selectTypeScriptVersion" })
              end,
              desc = "Select TS workspace version",
              ft = { "typescript", "javascript", "typescriptreact", "javascriptreact" },
            },
            {
              "<leader>lp",
              ft = "markdown",
              "<cmd>MarkdownPreviewToggle<cr>",
              desc = "Markdown Preview",
            },
            {
              "<leader>lh",
              "<cmd>ClangdSwitchSourceHeader<cr>",
              desc = "Switch Source/Header (C/C++)",
              ft = { "c", "cpp" },
            },
            {
              "<leader>lp",
              function()
                local params = vim.lsp.util.make_position_params()
                LazyVim.lsp.execute({
                  command = "manipulatePipes:serverid",
                  arguments = { "toPipe", params.textDocument.uri, params.position.line, params.position.character },
                })
              end,
              ft = { "heex", "elixir" },
              desc = "To Pipe",
            },
            {
              "<leader>lP",
              function()
                local params = vim.lsp.util.make_position_params()
                LazyVim.lsp.execute({
                  command = "manipulatePipes:serverid",
                  arguments = { "fromPipe", params.textDocument.uri, params.position.line, params.position.character },
                })
              end,
              ft = { "heex", "elixir" },
              desc = "From Pipe",
            },
          },
        },
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      vim.env.ESLINT_D_PPID = vim.fn.getpid()
    end,
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
