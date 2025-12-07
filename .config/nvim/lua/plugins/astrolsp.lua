local function filter(arr, fn)
  if type(arr) ~= "table" then return arr end

  local filtered = {}
  for k, v in pairs(arr) do
    if fn(v, k, arr) then table.insert(filtered, v) end
  end

  return filtered
end

local function filterReactDTS(value) return string.match(value.targetUri or value.uri, "%/index.d.ts") == nil end

---@type LazySpec
return {
  "AstroNvim/astrolsp",
  optional = true,
  ---@type AstroLSPOpts
  opts = {
    -- Configuration table of features provided by AstroLSP
    features = {
      -- codelens = true, -- enable/disable codelens refresh on start
      inlay_hints = false, -- enable/disable inlay hints on start
      signature_help = true,
      -- semantic_tokens = true, -- enable/disable semantic token highlighting
    },
    -- enable servers that you already have installed without mason
    -- servers = {},
    -- customize language server configuration options passed to `lspconfig`
    ---@diagnostic disable: missing-fields
    config = {
      djlsp = {},
      zls = {},
      emmet_language_server = {
        filetypes = {
          "templ",
        },
      },
      clangd = { capabilities = { offsetEncoding = "utf-8" } },
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
        handlers = {
          ["textDocument/definition"] = function(err, result, method, ...)
            if vim.islist(result) and #result > 1 then
              local filtered_result = filter(result, filterReactDTS)
              return vim.lsp.handlers["textDocument/definition"](err, filtered_result, method, ...)
            end

            vim.lsp.handlers["textDocument/definition"](err, result, method, ...)
          end,
        },
      },
      emmet_ls = false,
      codebook = {
        cmd = { "codebook-lsp", "serve" },
        filetypes = {
          "c",
          "cpp",
          "css",
          "scss",
          "html",
          "javascript",
          "javascriptreact",
          "typescript",
          "typescriptreact",
          "go",
          "rust",
          "toml",
          "markdown",
          "python",
        },
        root_markers = { ".git", "codebook.toml", ".codebook.toml" },
      },
    },
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
