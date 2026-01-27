---@type LazySpec
return {
  "AstroNvim/astrolsp",
  ---@type AstroLSPOpts
  opts = {
    features = {
      codelens = true, -- enable/disable codelens refresh on start
      inlay_hints = false, -- enable/disable inlay hints on start
      semantic_tokens = true, -- enable/disable semantic token highlighting
      signature_help = true,
    },
    -- enable servers that you already have installed without mason
    servers = {},
    -- customize language server configuration options passed to `lspconfig`
    ---@diagnostic disable: missing-fields
    config = {
      clangd = { capabilities = { offsetEncoding = "utf-8" } },
      emmet_language_server = { filetypes = { "templ" } },
      emmet_ls = false,
      vtsls = {
        settings = {
          complete_function_calls = true,
          vtsls = {
            enableMoveToFileCodeAction = true,
            autoUseWorkspaceTsdk = true,
            tsserver = {
              globalPlugins = {
                {
                  configNamespace = "typescript",
                  name = "typescript-svelte-plugin",
                  location = vim.fn.stdpath "data"
                    .. "/mason/packages/svelte-language-server/node_modules/typescript-svelte-plugin",
                  enableForWorkspaceTypeScriptVersions = true,
                },
              },
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
        -- handlers = {
        --   ["textDocument/definition"] = function(err, result, method, ...)
        --     if vim.islist(result) and #result > 1 then
        --       local filtered_result = filter(result, filterReactDTS)
        --       return vim.lsp.handlers["textDocument/definition"](err, filtered_result, method, ...)
        --     end
        --
        --     vim.lsp.handlers["textDocument/definition"](err, result, method, ...)
        --   end,
        -- },
      },
    },
    -- customize how language servers are attached
    handlers = {
      -- a function without a key is simply the default handler, functions take two parameters, the server name and the configured options table for that server
      -- function(server, opts) require("lspconfig")[server].setup(opts) end

      -- the key is the server that is being setup with `lspconfig`
      -- rust_analyzer = false, -- setting a handler to false will disable the set up of that language server
      -- pyright = function(_, opts) require("lspconfig").pyright.setup(opts) end -- or a custom handler function can be passed
    },
    -- Configure buffer local auto commands to add when attaching a language server
    autocmds = {
      -- first key is the `augroup` to add the auto commands to (:h augroup)
      lsp_codelens_refresh = {
        -- Optional condition to create/delete auto command group
        -- can either be a string of a client capability or a function of `fun(client, bufnr): boolean`
        -- condition will be resolved for each client on each execution and if it ever fails for all clients,
        -- the auto commands will be deleted for that buffer
        cond = "textDocument/codeLens",
        -- cond = function(client, bufnr) return client.name == "lua_ls" end,
        -- list of auto commands to set
        {
          -- events to trigger
          event = { "InsertLeave", "BufEnter" },
          -- the rest of the autocmd options (:h nvim_create_autocmd)
          desc = "Refresh codelens (buffer)",
          callback = function(args)
            if require("astrolsp").config.features.codelens then vim.lsp.codelens.refresh { bufnr = args.buf } end
          end,
        },
      },
    },
    -- mappings to be set up on attaching of a language server
    mappings = {
      n = {
        -- a `cond` key can provided as the string of a server capability to be required to attach, or a function with `client` and `bufnr` parameters from the `on_attach` that returns a boolean
        gD = {
          function() vim.lsp.buf.declaration() end,
          desc = "Declaration of current symbol",
          cond = "textDocument/declaration",
        },
        ["<Leader>le"] = {
          function() vim.cmd "EslintFixAll" end,
          desc = "Eslint Fix",
          cond = function(client) return client.name == "eslint" end,
        },
        ["<Leader>lo"] = {
          function()
            local clients = vim.lsp.get_clients { bufnr = 0, name = "vtsls" }
            local is_vtsls = #clients ~= 0

            if is_vtsls then
              vim.cmd "VtsExec organize_imports"
              return
            end

            vim.lsp.buf.code_action {
              apply = true,
              context = {
                only = { "source.organizeImports" },
                diagnostics = {},
              },
            }
          end,
          desc = "Organize Imports",
        },
        ["<Leader>lv"] = {
          function() vim.cmd "VtsExec select_ts_version" end,
          desc = "Change TS version",
          cond = function(client) return client.name == "vtsls" end,
        },
        gro = {
          function()
            local is_available = require("astrocore").is_available

            if is_available "fzf-lua" then
              require("fzf-lua").lsp_document_symbols { fzf_cli_args = "--tiebreak=end,index" }
            elseif is_available "snacks.nvim" then
              require("snacks.picker").lsp_symbols()
            else
              vim.lsp.buf.document_symbol()
            end
          end,
          desc = "Document symbols",
          cond = "textDocument/documentSymbol",
        },
        ["<leader>ls"] = {
          function()
            local is_available = require("astrocore").is_available

            if is_available "fzf-lua" then
              require("fzf-lua").lsp_document_symbols { fzf_cli_args = "--tiebreak=end,index" }
            elseif is_available "snacks.nvim" then
              require("snacks.picker").lsp_symbols()
            else
              vim.lsp.buf.document_symbol()
            end
          end,
          desc = "Document symbols",
          cond = "textDocument/documentSymbol",
        },
        grO = {
          function()
            local is_available = require("astrocore").is_available

            if is_available "fzf-lua" then
              require("fzf-lua").lsp_workspace_symbols { fzf_cli_args = "--nth 1..2" }
            elseif is_available "snacks.nvim" then
              require("snacks.picker").lsp_workspace_symbols()
            else
              vim.lsp.buf.workspace_symbol()
            end
          end,
          desc = "Workspace symbols",
          cond = "workspace/symbol",
        },
        ["<Leader>uY"] = {
          function() require("astrolsp.toggles").buffer_semantic_tokens() end,
          desc = "Toggle LSP semantic highlight (buffer)",
          cond = function(client)
            return client.supports_method "textDocument/semanticTokens/full" and vim.lsp.semantic_tokens ~= nil
          end,
        },
      },
    },
    -- A custom `on_attach` function to be run after the default `on_attach` function
    -- takes two parameters `client` and `bufnr`  (`:h lspconfig-setup`)
    -- on_attach = function(client, bufnr)
    -- this would disable semanticTokensProvider for all clients
    -- client.server_capabilities.semanticTokensProvider = nil
    -- end,
  },
}
