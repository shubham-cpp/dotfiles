return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local lsp_file_operations = require("core.lsp_file_operations")

      vim.lsp.config("*", {
        capabilities = vim.tbl_deep_extend(
          "force",
          vim.lsp.protocol.make_client_capabilities(),
          lsp_file_operations.capabilities()
        ),
      })

      local diag_icons = require("core.icons")

      --- @param diagnostic? vim.Diagnostic
      --- @param bufnr integer
      local function on_jump(diagnostic, bufnr)
        if not diagnostic then
          return
        end

        vim.diagnostic.show(
          diagnostic.namespace,
          bufnr,
          { diagnostic },
          { float = {
            bufnr = bufnr,
            scope = "cursor",
          } }
        )
      end
      vim.diagnostic.config({
        severity_sort = true,
        virtual_text = false,
        float = true,
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = diag_icons.error,
            [vim.diagnostic.severity.WARN] = diag_icons.warn,
            [vim.diagnostic.severity.INFO] = diag_icons.info,
            [vim.diagnostic.severity.HINT] = diag_icons.hint,
          },
        },
        jump = { on_jump = on_jump },
      })

      local function lsp_pick(method, fallback)
        local ok, fzf = pcall(require, "fzf-lua")
        if ok and fzf[method] then
          fzf[method]()
        else
          fallback()
        end
      end

      local function smart_rename()
        local ok, autotag = pcall(require, "ts-autotag")
        if ok and autotag.rename(nil, true) then
          return
        end
        vim.lsp.buf.rename()
      end

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local bufnr = args.buf
          local client = vim.lsp.get_client_by_id(args.data.client_id)

          if vim.b[bufnr].bigfile then
            if client then
              vim.schedule(function()
                if vim.api.nvim_buf_is_valid(bufnr) and not client:is_stopped() then
                  pcall(vim.lsp.buf_detach_client, bufnr, client.id)
                end
              end)
            end
            return
          end

          vim.keymap.set("n", "gd", function()
            lsp_pick("lsp_definitions", vim.lsp.buf.definition)
          end, { buffer = bufnr, desc = "Goto Definition" })
          vim.keymap.set("n", "grr", function()
            lsp_pick("lsp_references", vim.lsp.buf.references)
          end, { buffer = bufnr, desc = "References" })
          vim.keymap.set("n", "gri", function()
            lsp_pick("lsp_implementations", vim.lsp.buf.implementation)
          end, { buffer = bufnr, desc = "Goto Implementation" })
          vim.keymap.set("n", "grt", function()
            lsp_pick("lsp_typedefs", vim.lsp.buf.type_definition)
          end, { buffer = bufnr, desc = "Goto Type Definition" })
          vim.keymap.set("n", "grn", smart_rename, { buffer = bufnr, desc = "Rename" })

          local function bufmap(mode, lhs, rhs, desc)
            vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
          end

          bufmap({ "n", "v" }, "<leader>la", vim.lsp.buf.code_action, "Code Action")
          bufmap("n", "<leader>lr", smart_rename, "Rename")
          bufmap("n", "<leader>lw", function()
            lsp_pick("lsp_document_symbols", vim.lsp.buf.document_symbol)
          end, "Document Symbols")
          bufmap("n", "<leader>lW", function()
            lsp_pick("lsp_live_workspace_symbols", vim.lsp.buf.workspace_symbol)
          end, "Workspace Symbols")

          if not client then
            return
          end
          if client:supports_method("textDocument/codeLens") then
            bufmap("n", "<leader>lc", vim.lsp.codelens.run, "Run Codelens")
          end

          if client.name == "vtsls" then
            bufmap("n", "grs", function()
              require("vtsls").commands.goto_source_definition(0)
            end, "Goto Source Definition")
            bufmap("n", "gro", function()
              vim.lsp.buf.code_action({
                context = { only = { "source.organizeImports" } },
                apply = true,
              })
            end, "Organize Imports")
            bufmap("n", "<leader>lM", function()
              vim.lsp.buf.code_action({
                context = { only = { "source.addMissingImports" } },
                apply = true,
              })
            end, "Add Missing Imports")
            bufmap("n", "<leader>lu", function()
              vim.lsp.buf.code_action({
                context = { only = { "source.removeUnused" } },
                apply = true,
              })
            end, "Remove Unused Imports")
            bufmap("n", "<leader>lF", function()
              vim.lsp.buf.code_action({
                context = { only = { "source.fixAll" } },
                apply = true,
              })
            end, "Fix All")
            bufmap("n", "<leader>lV", function()
              vim.cmd("VtslsSelectTsVersion")
            end, "Select TS Version")
          end

          if client.name == "eslint" then
            bufmap("n", "<leader>le", "<cmd>LspEslintFixAll<cr>", "Eslint Fix All")
          end

          if client:supports_method("textDocument/foldingRange") then
            local win = vim.api.nvim_get_current_win()
            vim.wo[win].foldmethod = "expr"
            vim.wo[win].foldexpr = "v:lua.vim.lsp.foldexpr()"
          end

          if
            client:supports_method("textDocument/inlayHint")
            and not vim.b[bufnr].bigfile
            and vim.bo[bufnr].filetype ~= "vue"
          then
            vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
          end
        end,
      })

      vim.lsp.config("emmylua_ls", {
        settings = {
          Lua = {
            runtime = { version = "LuaJIT", path = { "?.lua", "?/init.lua" }, pathStrict = true },
            diagnostics = { globals = { "vim" } },
            workspace = { checkThirdParty = false, library = { vim.env.VIMRUNTIME } },
            hint = { enable = true },
            type = { castNumberToPercent = true },
          },
        },
      })
      vim.lsp.enable("emmylua_ls")

      vim.lsp.config("gopls", {
        settings = {
          gopls = {
            analyses = {
              unusedparams = true,
              unusedwrite = true,
              unusedvariable = true,
              fieldalignment = false,
              nilness = true,
              shadow = true,
            },
            staticcheck = true,
            gofumpt = true,
            completeUnimported = true,
            usePlaceholders = true,
            codelenses = {
              gc_details = false,
              generate = true,
              run_govulncheck = true,
              test = true,
              tidy = true,
              upgrade_dependency = true,
              vendor = true,
            },
            hints = {
              assignVariableTypes = true,
              compositeLiteralFields = true,
              compositeLiteralTypes = true,
              constantValues = true,
              functionTypeParameters = true,
              parameterNames = true,
              rangeVariableTypes = true,
            },
            vulncheck = "Imports",
            directoryFilters = { "-.git", "-node_modules" },
          },
        },
      })
      vim.lsp.enable("gopls")

      vim.lsp.config("html", {
        settings = {
          html = {
            format = { enable = false },
            hover = { documentation = true, references = true },
            validate = { scripts = true, styles = true },
            autoClosingTags = true,
          },
        },
      })
      vim.lsp.enable("html")

      vim.lsp.config("cssls", {
        settings = {
          css = { validate = true, lint = { unknownAtRules = "ignore" } },
          scss = { validate = true, lint = { unknownAtRules = "ignore" } },
          less = { validate = true, lint = { unknownAtRules = "ignore" } },
        },
      })
      vim.lsp.enable("cssls")
      vim.lsp.enable("eslint")
      vim.lsp.enable("css_variables")
      vim.lsp.enable("cssmodules_ls")
      vim.lsp.enable("docker_compose_language_service")
      vim.lsp.enable("docker_language_server")

      vim.lsp.config("taplo", {
        settings = {
          evenBetterToml = {
            schema = { enabled = true },
            formatter = { alignEntries = true, columnWidth = 80, trailingNewline = true },
          },
        },
      })
      vim.lsp.enable("taplo")

      vim.lsp.config("bashls", {
        settings = {
          bashIde = {
            globPattern = "*@(.sh|.inc|.bash|.command)",
            highlightParsingErrors = true,
          },
        },
      })
      vim.lsp.enable("bashls")

      vim.lsp.config("svelte", {
        settings = {
          typescript = {
            updateImportsOnFileMove = { enabled = "always" },
            inlayHints = {
              parameterNames = { enabled = "all" },
              parameterTypes = { enabled = true },
              variableTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              enumMemberValues = { enabled = true },
            },
          },
          javascript = {
            updateImportsOnFileMove = { enabled = "always" },
            inlayHints = {
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              variableTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              enumMemberValues = { enabled = true },
            },
          },
        },
      })
      vim.lsp.enable("svelte")
      vim.lsp.enable("vue_ls")
      vim.lsp.enable("emmet_language_server")

      vim.lsp.config("pyrefly", {
        settings = {
          python = {
            pyrefly = {
              typeCheckingMode = "default",
              diagnosticMode = "workspace",
              streamDiagnostics = true,
              analysis = {
                showHoverGoToLinks = true,
                inlayHints = {
                  callArgumentNames = "partial",
                  functionReturnTypes = true,
                  variableTypes = true,
                  pytestParameters = false,
                },
              },
            },
            analysis = { completeFunctionParens = true },
          },
        },
      })
      vim.lsp.enable("pyrefly")
    end,
  },
  {
    "mason-org/mason.nvim",
    config = function()
      require("mason").setup({
        PATH = "prepend",
        ui = {
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      })
      local tools = {
        "emmylua_ls",
        "vtsls",
        "gopls",
        "html-lsp",
        "css-lsp",
        "json-lsp",
        "yaml-language-server",
        "bash-language-server",
        "rust-analyzer",
        "taplo",
        "stylua",
        "shfmt",
        "prettierd",
        "eslint-lsp",
        "stylelint",
        "golangci-lint",
        "pyrefly",
        "cssmodules-language-server",
        "css-variables-language-server",
        "docker-compose-language-service",
        "docker-language-server",
        "svelte-language-server",
        "vue-language-server",
        "emmet-language-server",
        "tree-sitter-cli",
      }
      local mr = require("mason-registry")
      local function ensure_installed()
        for _, tool in ipairs(tools) do
          local ok, p = pcall(mr.get_package, tool)
          if ok and not p:is_installed() then
            vim.notify(("Installing Mason package: %s"):format(tool), vim.log.levels.INFO)
            p:install()
          end
        end
      end

      vim.api.nvim_create_user_command("MasonSyncTools", function()
        if mr.refresh then
          mr.refresh(ensure_installed)
        else
          ensure_installed()
        end
      end, { desc = "Install configured Mason tools" })

      vim.keymap.set("n", "<leader>lm", "<cmd>Mason<cr>", { desc = "Mason" })
    end,
  },
  {
    "yioneko/nvim-vtsls",
    config = function()
      local function filter_react_dts(value)
        return not (value.targetUri or value.uri or ""):match("/index%.d%.ts")
      end

      local function definition_handler(err, result, method, ...)
        if vim.islist(result) and #result > 1 then
          result = vim.tbl_filter(filter_react_dts, result)
        end
        return vim.lsp.handlers["textDocument/definition"](err, result, method, ...)
      end

      local function resolve_ts_plugin(mason_pkg, plugin_subpath)
        local project_root = vim.fs.root(0, "node_modules")
        if project_root then
          local project_path = project_root .. "/node_modules/" .. plugin_subpath
          if vim.fn.isdirectory(project_path) == 1 then
            return project_path
          end
        end
        local mason_path = vim.fn.stdpath("data")
          .. "/mason/packages/"
          .. mason_pkg
          .. "/node_modules/"
          .. plugin_subpath
        if vim.fn.isdirectory(mason_path) == 1 then
          return mason_path
        end
        return nil
      end
      vim.lsp.config("vtsls", {
        filetypes = {
          "javascript",
          "javascriptreact",
          "javascript.jsx",
          "typescript",
          "typescriptreact",
          "typescript.tsx",
          "vue",
        },
        settings = {
          vtsls = {
            autoUseWorkspaceTsdk = true,
            enableMoveToFileCodeAction = true,
            experimental = {
              completion = {
                enableServerSideFuzzyMatch = true,
                entriesLimit = 100,
              },
              maxInlayHintLength = 30,
            },
            tsserver = {
              maxTsServerMemory = 8192,
              globalPlugins = (function()
                local plugins = {}
                local svelte_path = resolve_ts_plugin("svelte-language-server", "typescript-svelte-plugin")
                if svelte_path then
                  table.insert(plugins, {
                    name = "typescript-svelte-plugin",
                    location = svelte_path,
                    enableForWorkspaceTypeScriptVersions = true,
                  })
                end
                local vue_path = resolve_ts_plugin("vue-language-server", "@vue/language-server")
                if vue_path then
                  table.insert(plugins, {
                    name = "@vue/typescript-plugin",
                    location = vue_path,
                    languages = { "vue" },
                    configNamespace = "typescript",
                    enableForWorkspaceTypeScriptVersions = true,
                  })
                end
                return plugins
              end)(),
            },
          },
          typescript = {
            preferences = {
              includePackageJsonAutoImports = "auto",
            },
            inlayHints = {
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              variableTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              enumMemberValues = { enabled = true },
            },
            updateImportsOnFileMove = "always",
          },
          javascript = {
            preferences = {
              includePackageJsonAutoImports = "auto",
            },
            inlayHints = {
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              variableTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              enumMemberValues = { enabled = true },
            },
            updateImportsOnFileMove = "always",
          },
        },
        handlers = {
          ["textDocument/definition"] = definition_handler,
        },
      })
      vim.lsp.enable("vtsls")
    end,
  },
  {
    "b0o/SchemaStore.nvim",
    ft = { "json", "jsonc", "yaml", "yml" },
    config = function()
      vim.lsp.config("jsonls", {
        settings = {
          json = {
            schemas = require("schemastore").json.schemas(),
            validate = { enable = true },
          },
        },
      })
      vim.lsp.enable("jsonls")
      vim.lsp.config("yamlls", {
        settings = {
          yaml = {
            schemaStore = { enable = false, url = "" },
            schemas = require("schemastore").yaml.schemas(),
          },
        },
      })
      vim.lsp.enable("yamlls")
    end,
  },
  {
    "mrcjkb/rustaceanvim",
    version = vim.version.range("^9"),
    init = function()
      vim.g.rustaceanvim = {
        server = {
          default_settings = {
            ["rust-analyzer"] = {
              cargo = { features = "all" },
              check = { command = "clippy" },
              procMacro = { enable = true },
            },
          },
        },
        dap = { autoload_configurations = false },
      }
    end,
  },
  {
    "Saecki/crates.nvim",
    event = { "BufRead Cargo.toml", "BufNewFile Cargo.toml" },
    opts = {
      smart_insert = true,
      autoload = true,
      autoupdate = true,
      completion = { crates = { enabled = true, min_chars = 3, max_results = 8 } },
      lsp = { enabled = false, actions = false, completion = false, hover = false },
    },
  },
}
