---@type LazySpec
return {
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "ruff", "basedpyright" }, },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "python", "pymanifest" } },
  },
  {
    'neovim/nvim-lspconfig',
    opts = function(_, opts)
      opts.servers.ruff = {
        cmd_env = { RUFF_TRACE = "messages" },
        init_options = { settings = { logLevel = "error", }, },
        on_attach = function(client, buffer)
          client.server_capabilities.hoverProvider = false
          vim.keymap.set("n", "<leader>lo", function()
            vim.lsp.buf.code_action({
              apply = true,
              context = {
                only = { "source.organizeImports" },
                diagnostics = {},
              },
            })
          end, { desc = "Organize Imports", buffer = buffer })
        end
      }
      opts.servers.basedpyright = {
        before_init = function(_, c)
          if not c.settings then c.settings = {} end
          if not c.settings.python then c.settings.python = {} end
          c.settings.python.pythonPath = vim.fn.exepath "python"
        end,
        settings = {
          basedpyright = {
            analysis = {
              typeCheckingMode = "basic",
              autoImportCompletions = true,
              diagnosticSeverityOverrides = {
                reportUnusedImport = "information",
                reportUnusedFunction = "information",
                reportUnusedVariable = "information",
                reportGeneralTypeIssues = "none",
                reportOptionalMemberAccess = "none",
                reportOptionalSubscript = "none",
                reportPrivateImportUsage = "none",
              },
            },
          },
        },
      }
    end,
  }
}
