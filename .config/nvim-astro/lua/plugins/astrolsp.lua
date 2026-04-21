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
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      features = {
        codelens = true,
        inlay_hints = true,
        semantic_tokens = true,
        signature_help = true,
      },
      config = {
        clangd = { capabilities = { offsetEncoding = "utf-8" } },
        emmet_language_server = { filetypes = { "templ" } },
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
        -- emmylua_ls = {
        --   settings = {
        --     Lua = {
        --       runtime = {
        --         version = "LuaJIT",
        --         requirePattern = {
        --           "lua/?/init.lua",
        --           "lua/?.lua",
        --           "?/lua/?/init.lua",
        --           "?/lua/?.lua",
        --           "?.lua",
        --         },
        --       },
        --       diagnostics = {
        --         enable = true,
        --         globals = { "vim" },
        --         disable = { "undefined-doc-param" },
        --       },
        --       workspace = {
        --         -- library = {
        --         --   vim.env.VIMRUNTIME,
        --         --   vim.fn.stdpath "data" .. "/lazy",
        --         --   vim.fn.stdpath "config" .. "/lua",
        --         -- },
        --         library = vim.tbl_map(function(p) return p .. "/" end, vim.api.nvim_list_runtime_paths()),
        --         ignoreDir = { ".git", "node_modules", ".cache" },
        --       },
        --       hint = {
        --         enable = true,
        --         paramHint = true,
        --         indexHint = true,
        --         localHint = true,
        --         overrideHint = true,
        --       },
        --       completion = {
        --         enable = true,
        --         autoRequire = true,
        --         callSnippet = false,
        --       },
        --       strict = {
        --         arrayIndex = true,
        --         requirePath = false,
        --         typeCall = false,
        --       },
        --     },
        --   },
        -- },
      },
      handlers = {
        emmet_ls = false,
        -- lua_ls = false,
      },
      autocmds = {},
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
          ["gro"] = {
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
          ["grv"] = {
            function() vim.cmd "VtsExec select_ts_version" end,
            desc = "Change TS version",
            cond = function(client) return client.name == "vtsls" end,
          },
        },
      },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed =
        require("astrocore").list_insert_unique(opts.ensure_installed, { "emmet_language_server" })
      if type(opts.ensure_installed) == "table" then
        opts.ensure_installed = vim.tbl_filter(function(s) return s ~= "emmet_ls" end, opts.ensure_installed)
      end
    end,
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        opts.ensure_installed =
          require("astrocore").list_insert_unique(opts.ensure_installed, { "emmet-language-server" })
        opts.ensure_installed = vim.tbl_filter(function(s) return s ~= "emmet-ls" end, opts.ensure_installed)
      end
    end,
  },
}
