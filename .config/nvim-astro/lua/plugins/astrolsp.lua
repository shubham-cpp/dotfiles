---@type LazySpec
return {
  "AstroNvim/astrolsp",
  ---@type AstroLSPOpts
  opts = {
    features = {
      inlay_hints = false,
    },
    -- enable servers that you already have installed without mason
    -- servers = { },
    -- customize language server configuration options passed to `lspconfig`
    ---@diagnostic disable: missing-fields
    config = {
      clangd = { capabilities = { offsetEncoding = "utf-8" } },
      vtsls = {
        settings = {
          complete_function_calls = true,
          vtsls = {
            enableMoveToFileCodeAction = true,
            autoUseWorkspaceTsdk = true,
            experimental = {
              maxInlayHintLength = 30,
              -- completion = {
              --   enableServerSideFuzzyMatch = true,
              --   entriesLimit = 30,
              -- },
            },
          },
          typescript = {
            updateImportsOnFileMove = { enabled = "always" },
            suggest = { completeFunctionCalls = true },
            inlayHints = {
              enumMemberValues = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              variableTypes = { enabled = false },
            },
          },
          javascript = {
            updateImportsOnFileMove = { enabled = "always" },
            suggest = { completeFunctionCalls = true },
            inlayHints = {
              enumMemberValues = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              variableTypes = { enabled = false },
            },
          },
        },
      },
    },
    -- customize how language servers are attached
    handlers = {
      tailwindcss = function(server, opts)
        local default_attach = opts.on_attach
        opts.on_attach = function(client, bufnr)
          default_attach(client, bufnr)
          client.server_capabilities.completionProvider.triggerCharacters =
            { '"', "'", "`", ".", "(", "[", "!", "/", ":" }
        end
        require("lspconfig")[server].setup(opts)
      end,
    },
    autocmds = {
      eslint_auto_fix = {
        cond = function(client) return client.name == "eslint" end,
        {
          event = "BufWritePre",
          desc = "Run Eslint fix on save",
          command = "EslintFixAll",
        },
      },
    },
    -- mappings to be set up on attaching of a language server
    mappings = {
      n = {
        ["<Leader>le"] = {
          function() vim.cmd "EslintFixAll" end,
          desc = "Eslint Fix",
          cond = function(client) return client.name == "eslint" end,
        },
        ["<Leader>lo"] = {
          function() vim.cmd "VtsExec organize_imports" end,
          desc = "Organize Imports",
          cond = function(client) return client.name == "vtsls" end,
        },
        ["<Leader>lv"] = {
          function() vim.cmd "VtsExec select_ts_version" end,
          desc = "Change TS version",
          cond = function(client) return client.name == "vtsls" end,
        },
        gro = {
          function()
            local ok, picker = pcall(require, "snacks.picker")
            if ok then
              picker.lsp_symbols()
            else
              vim.lsp.buf.document_symbol()
            end
          end,
          desc = "Document symbols",
          cond = "textDocument/documentSymbol",
        },
        grO = {
          function()
            local ok, picker = pcall(require, "snacks.picker")
            if ok then
              picker.lsp_workspace_symbols()
            else
              vim.lsp.buf.workspace_symbol()
            end
          end,
          desc = "Workspace symbols",
          cond = "workspace/symbol",
        },
      },
    },
  },
}
