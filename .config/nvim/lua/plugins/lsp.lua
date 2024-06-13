---@type LazySpec
return {
  'neovim/nvim-lspconfig',
  event = 'BufReadPre',
  dependencies = {
    {
      'AstroNvim/astrolsp',
      dependencies = {
        { 'b0o/schemastore.nvim', ft = { 'json', 'jsonc', 'json5', 'yaml' }, version = false },
        {
          'pmizio/typescript-tools.nvim',
          dependencies = 'nvim-lua/plenary.nvim',
          ft = { 'javascript', 'typescript', 'javascriptreact', 'typescriptreact' },
          enabled = false,
        },
        {
          'yioneko/nvim-vtsls',
          lazy = true,
          opts = {},
          config = function(_, opts)
            require('vtsls').config(opts)
          end,
        },
        {
          'mrcjkb/rustaceanvim',
          enabled = false,
          version = '^4', -- Recommended
          lazy = false, -- This plugin is already lazy
        },
        {
          'SmiteshP/nvim-navic',
          init = function()
            vim.g.navic_silence = true
          end,
          opts = {
            highlight = true,
            depth_limit = 5,
            lazy_update_context = true,
          },
        },
        {
          'elixir-tools/elixir-tools.nvim',
          version = '*',
          event = { 'BufReadPre', 'BufNewFile' },
          dependencies = {
            'nvim-lua/plenary.nvim',
          },
        },
      },
      ---@type AstroLSPConfig
      opts = {
        -- Configuration table of features provided by AstroLSP
        features = {
          autoformat = true, -- enable or disable auto formatting on start
          codelens = true, -- enable/disable codelens refresh on start
          inlay_hints = false, -- enable/disable inlay hints on start
          semantic_tokens = true, -- enable/disable semantic token highlighting
        },
        -- Configure buffer local auto commands to add when attaching a language server
        autocmds = {
          -- first key is the `augroup` (:h augroup)
          lsp_document_highlight = {
            cond = 'textDocument/documentHighlight',
            -- list of auto commands to set
            {
              -- events to trigger
              event = { 'CursorHold', 'CursorHoldI' },
              -- the rest of the autocmd options (:h nvim_create_autocmd)
              desc = 'Document Highlighting',
              callback = vim.lsp.buf.document_highlight,
            },
            {
              event = { 'CursorMoved', 'CursorMovedI', 'BufLeave' },
              desc = 'Document Highlighting Clear',
              callback = vim.lsp.buf.clear_references,
            },
          },
          svelte_auto_format = {
            cond = function(client)
              return client.name == 'svelte'
            end,
            {
              event = 'BufWritePost',
              pattern = { '*.js', '*.ts' },
              desc = 'Svelte auto format',
              callback = function(ctx)
                -- get the current lsp client
                -- and notify the server that the file has been changed
                vim.lsp.buf_notify(0, '$/onDidChangeTsOrJsFile', { uri = ctx.file })
                -- client.notify("$/onDidChangeTsOrJsFile", { uri = ctx.file })
              end,
            },
          },
        },
        -- Configure buffer local user commands to add when attaching a language server
        commands = {
          Format = {
            function()
              vim.lsp.buf.format({ async = true })
            end,
            cond = 'textDocument/formatting',
            desc = 'Format file with LSP',
          },
        },
        -- Configure default capabilities for language servers (`:h vim.lsp.protocol.make_client.capabilities()`)
        capabilities = {
          textDocument = { foldingRange = { dynamicRegistration = false, lineFoldingOnly = true } },
        },
        -- Configure language servers for `lspconfig` (`:h lspconfig-setup`)
        config = {
          lua_ls = {
            settings = {
              Lua = {
                diagnostics = {
                  enable = true,
                  globals = { 'vim', 'describe' },
                  disable = { 'lowercase-global' },
                },
                workspace = {
                  checkThirdParty = false,
                  library = {
                    vim.env.VIMRUNTIME,
                  },
                },
                runtime = { version = 'LuaJIT' },
                telemetry = { enable = false },
                completion = { callSnippet = 'Replace' },
                hint = { enable = true, arrayIndex = 'Disable' },
              },
            },
          },
          clangd = {
            capabilities = {
              offsetEncoding = { 'utf-16' },
              clangdInlayHintsProvider = true,
            },
            settings = {
              InlayHints = {
                Enabled = true,
                ParameterNames = true,
                DeducedTypes = true,
              },
            },
            cmd = {
              -- 'clangd',
              -- '--background-index',
              -- '--clang-tidy',
              -- '--suggest-missing-includes',
              -- '--header-insertion=iwyu',
              -- '--completion-style=detailed',
              -- '--function-arg-placeholders',
              -- '--pch-storage=memory',
              -- '--fallback-style=llvm',
              'clangd',
              '--background-index',
              '--clang-tidy',
              '--suggest-missing-includes',
              '--header-insertion-decorators',
              '--all-scopes-completion',
              '--cross-file-rename',
              '--log=info',
              '--completion-style=detailed',
              -- "--enable-config", -- clangd 11+ supports reading from .clangd configuration file
              '--offset-encoding=utf-16',
              '--header-insertion=never',
            },
          },
          gopls = {
            settings = {
              gopls = {
                experimentalPostfixCompletions = true,
                analyses = { unusedparams = true, shadow = true, nilness = true, unusedwrite = true },
                hints = {
                  assignVariableTypes = true,
                  compositeLiteralFields = true,
                  compositeLiteralTypes = true,
                  constantValues = true,
                  functionTypeParameters = true,
                  parameterNames = true,
                  rangeVariableTypes = true,
                },
                staticcheck = true,
                ui = {
                  completion = {
                    usePlaceholders = true,
                  },
                },
                codelenses = {
                  usePlaceholders = true,
                },
              },
            },
          },
          svelte = {
            settings = {
              svelte = {
                ['enable-ts-plugin'] = true,
                plugin = { svelte = { defaultScriptLanguage = 'typescript' } },
              },
            },
          },
          pyright = {
            settings = {
              python = {
                analysis = {
                  autoImportCompletions = true,
                  -- typeCheckingMode = 'basic',
                },
                exclude = { '**/node_modules', '**/__pycache__' },
              },
            },
          },
          nim_langserver = {
            settings = {
              nim = {
                nimsuggestPath = vim.fn.expand '~/.local/share/nim-2.0.4/bin/nimsuggest',
              },
            },
          },
          bashls = {
            settings = { bashIde = { highlightParsingErrors = true } },
          },
          cssls = {
            settings = {
              css = {
                validate = true,
                completion = {
                  triggerPropertyValueCompletion = true,
                  completePropertyWithSemicolon = true,
                },
                lint = {
                  compatibleVendorPrefixes = 'warn',
                  duplicateProperties = 'warn',
                  boxModel = 'warn',
                  unknownVendorSpecificProperties = 'warn',
                  float = 'error',
                },
              },
              scss = {
                validate = true,
                completion = {
                  triggerPropertyValueCompletion = true,
                  completePropertyWithSemicolon = true,
                },
                lint = {
                  compatibleVendorPrefixes = 'warn',
                  duplicateProperties = 'warn',
                  boxModel = 'warn',
                  unknownVendorSpecificProperties = 'warn',
                  float = 'error',
                },
              },
            },
          },
          rust_analyzer = {
            settings = {
              ['rust-analyzer'] = {
                imports = {
                  granularity = {
                    group = 'module',
                  },
                  prefix = 'self',
                },
                cargo = {
                  buildScripts = {
                    enable = true,
                  },
                },
                procMacro = {
                  enable = true,
                },
              },
            },
          },
        },
        -- Configuration of mappings added when attaching a language server during the core `on_attach` function
        -- The first key into the table is the vim map mode (`:h map-modes`), and the value is a table of entries to be passed to `vim.keymap.set` (`:h vim.keymap.set`):
        --   - The key is the first parameter or the vim mode (only a single mode supported) and the value is a table of keymaps within that mode:
        --     - The first element with no key in the table is the action (the 2nd parameter) and the rest of the keys/value pairs are options for the third parameter.
        --       There is also a special `cond` key which can either be a string of a language server capability or a function with `client` and `bufnr` parameters that returns a boolean of whether or not the mapping is added.
        mappings = {
          -- map mode (:h map-modes)
          n = {
            K = { vim.lsp.buf.hover, desc = 'Hover', cond = 'textDocument/hover' },
            gl = { vim.diagnostic.open_float, desc = 'Hover diagnostics' },
            gd = {
              vim.lsp.buf.definition,
              desc = 'Goto definition',
              cond = 'textDocument/definition',
            },
            gs = {
              function()
                vim.lsp.buf.code_action({
                  context = { only = { 'source.organizeImports' } },
                  apply = true,
                })
                vim.lsp.buf.code_action({ context = { only = { 'source.fixAll' } }, apply = true })
              end,
              desc = 'Oraganize imports',
              cond = 'textDocument/codeAction',
            },
            gi = {
              vim.lsp.buf.implementation,
              desc = 'Goto implementation',
              cond = 'textDocument/implementation',
            },
            -- condition for only server with declaration capabilities
            gD = { vim.lsp.buf.declaration, desc = 'Goto Declaration', cond = 'textDocument/declaration' },
            gt = {
              vim.lsp.buf.type_definition,
              desc = 'Type definition',
              cond = 'textDocument/typeDefinition',
            },
            ['g='] = {
              function()
                vim.cmd [[EslintFixAll]]
              end,
              cond = function(client)
                return client.name == 'eslint'
              end,
            },
            gr = {
              vim.lsp.buf.references,
              desc = 'Goto references',
              cond = 'textDocument/references',
            },
            [']e'] = {
              function()
                vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR })
              end,
              desc = 'Goto Next Error',
            },
            ['[e'] = {
              function()
                vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR })
              end,
              desc = 'Goto Next Error',
            },
            [']d'] = {
              vim.diagnostic.goto_next,
              desc = 'Goto Next diagnostic',
            },
            ['[d'] = {
              vim.diagnostic.goto_prev,
              desc = 'Goto Next diagnostic',
            },
            gw = {
              vim.lsp.buf.document_symbol,
              desc = 'Document symbols',
              cond = 'textDocument/documentSymbol',
            },
            gW = {
              vim.lsp.buf.workspace_symbol,
              desc = 'Workspace symbols',
              cond = 'workspace/symbol',
            },
            ['<F2>'] = {
              vim.lsp.buf.rename,
              desc = 'Rename',
              cond = 'textDocument/rename',
            },
            ['<leader>lr'] = {
              vim.lsp.buf.rename,
              desc = 'Rename',
              cond = 'textDocument/rename',
            },
            ['<leader>la'] = {
              vim.lsp.buf.code_action,
              desc = 'Code actions',
              cond = 'textDocument/codeAction',
            },
            ['<leader>lI'] = {
              function()
                vim.cmd 'LspInfo'
              end,
              desc = 'LspInfo',
            },
            -- condition with a full function with `client` and `bufnr`
            ['<leader>uY'] = {
              function()
                require('astrolsp.toggles').buffer_semantic_tokens()
              end,
              desc = 'Toggle LSP semantic highlight (buffer)',
              cond = function(client)
                return client.server_capabilities.semanticTokensProvider and vim.lsp.semantic_tokens
              end,
            },
            ['<leader>uI'] = {
              function()
                require('astrolsp.toggles').buffer_inlay_hints()
              end,
              desc = 'Toggle LSP semantic highlight (buffer)',
              -- cond = function(client)
              --   return client.server_capabilities.semanticTokensProvider and vim.lsp.semantic_tokens
              -- end,
            },
          },
          i = {
            ['<C-k>'] = {
              vim.lsp.buf.signature_help,
              desc = 'Signature help',
              cond = 'textDocument/signatureHelp',
            },
          },
        },

        handlers = {
          function(server, opts)
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)
            require('lspconfig')[server].setup(opts)
          end,
          tsserver = false,
          vtsls = function(server, opts)
            local default_attach = opts.on_attach
            opts.filetypes = {
              'javascript',
              'javascriptreact',
              'javascript.jsx',
              'typescript',
              'typescriptreact',
              'typescript.tsx',
              'vue',
            }
            opts.settings = {
              complete_function_calls = true,
              vtsls = {
                enableMoveToFileCodeAction = true,
                autoUseWorkspaceTsdk = true,
                experimental = {
                  completion = {
                    enableServerSideFuzzyMatch = true,
                  },
                },
                tsserver = {
                  globalPlugins = {
                    {
                      name = 'typescript-svelte-plugin',
                      location = require('mason-registry').get_package('svelte-language-server'):get_install_path()
                        .. '/node_modules/typescript-svelte-plugin',
                      enableForWorkspaceTypeScriptVersions = true,
                    },
                    {
                      name = '@vue/typescript-plugin',
                      location = require('mason-registry').get_package('vue-language-server'):get_install_path()
                        .. '/node_modules/@vue/language-server',
                      languages = { 'vue' },
                      configNamespace = 'typescript',
                      enableForWorkspaceTypeScriptVersions = true,
                    },
                  },
                },
              },
              typescript = {
                updateImportsOnFileMove = { enabled = 'always' },
                suggest = {
                  completeFunctionCalls = true,
                },
                inlayHints = {
                  enumMemberValues = { enabled = true },
                  functionLikeReturnTypes = { enabled = true },
                  parameterNames = { enabled = 'literals' },
                  parameterTypes = { enabled = true },
                  propertyDeclarationTypes = { enabled = true },
                  variableTypes = { enabled = false },
                },
              },
            }
            opts.settings.javascript = vim.tbl_deep_extend('force', {}, opts.settings.typescript, {})

            opts.on_attach = function(client, buffer)
              ---@diagnostic disable-next-line: redefined-local
              local map = function(lhs, rhs, opts)
                local o = {
                  buffer = buffer,
                  noremap = true,
                  desc = opts,
                }
                vim.keymap.set('n', lhs, rhs, o)
              end

              map('gD', function()
                require('vtsls').commands.goto_source_definition(buffer)
              end, 'Goto Source Definition')

              map('gR', function()
                require('vtsls').commands.file_references(buffer)
              end, 'File References')
              map('<leader>lo', function()
                require('vtsls').commands.organize_imports(buffer)
                require('vtsls').commands.add_missing_imports(buffer)
                require('vtsls').commands.remove_unused_imports(buffer)
              end, 'Organize Imports')
              map('<leader>lF', function()
                require('vtsls').commands.fix_all(buffer)
              end, 'Fix All')
              map('<leader>lv', function()
                require('vtsls').commands.select_ts_version(buffer)
              end, 'Select Version')
              map('<leader>lR', function()
                require('vtsls').commands.rename_file(buffer)
              end, 'Rename File')
              default_attach(client, buffer)
            end
            -- require('lspconfig.configs')[server].setup = require('vtsls').lspconfig
            require('lspconfig')[server].setup(opts)
          end,
          lua_ls = function(server, opts)
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)
            require('lspconfig')[server].setup(opts)
          end,
          jsonls = function(server, opts)
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)
            opts.on_new_config = function(new_config)
              new_config.settings.json.schemas = new_config.settings.json.schemas or {}
              vim.list_extend(new_config.settings.json.schemas, require('schemastore').json.schemas())
            end
            opts.settings = {
              json = {
                -- schemas = require('schemastore').json.schemas({}),
                validate = { enable = true },
                format = { enable = false },
              },
            }
            require('lspconfig')[server].setup(opts)
          end,
          yamlls = function(server, opts)
            local capabilities = vim.lsp.protocol.make_client_capabilities()
            capabilities.textDocument.foldingRange = {
              dynamicRegistration = false,
              lineFoldingOnly = true,
            }
            opts.capabilities = capabilities
            opts.on_new_config = function(new_config)
              new_config.settings.yaml.schemas = vim.tbl_deep_extend(
                'force',
                new_config.settings.yaml.schemas or {},
                require('schemastore').yaml.schemas()
              )
            end
            opts.settings = {
              redhat = { telemetry = { enabled = false } },
              yaml = {
                keyOrdering = false,
                format = {
                  enable = true,
                },
                validate = true,
                schemaStore = { enable = false, url = '' },
              },
            }
            require('lspconfig')[server].setup(opts)
          end,
          tailwindcss = function(server, opts)
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)

            opts.root_dir = require('lspconfig.util').root_pattern(
              'tailwind.config.js',
              'tailwind.config.cjs',
              'tailwind.config.mjs',
              'tailwind.config.ts',
              'postcss.config.js',
              'postcss.config.cjs',
              'postcss.config.mjs',
              'postcss.config.ts'
            )
            opts.settings = {
              tailwindCSS = {
                emmetCompletions = true,
                validate = 'error',
              },
            }
            opts.single_file_support = false
            require('lspconfig')[server].setup(opts)
          end,
          elixir_ls = function(_, opts)
            local elixir = require 'elixir'
            local elixirls = require 'elixir.elixirls'

            elixir.setup({
              nextls = { enable = false },
              credo = { enable = true },
              elixirls = {
                enable = true,
                capabilities = opts.capabilities,
                settings = elixirls.settings({
                  dialyzerEnabled = true,
                  enableTestLenses = true,
                }),
                on_attach = function(client, bufnr)
                  opts.on_attach(client, bufnr)
                  vim.keymap.set('n', '<space>lp', ':ElixirFromPipe<cr>', { buffer = true, noremap = true })
                  vim.keymap.set('n', '<space>lP', ':ElixirToPipe<cr>', { buffer = true, noremap = true })
                  vim.keymap.set('v', '<space>lm', ':ElixirExpandMacro<cr>', { buffer = true, noremap = true })
                end,
              },
            })
          end,
          eslint = false,
          efm = false,
          zk = false,
        },
        -- A list like table of servers that should be setup, useful for enabling language servers not installed with Mason.
        servers = {
          'gleam',
          'zls',
          'roc_ls',
          'nim_langserver' --[[, 'rust_analyzer' ]],
        },
        on_attach = function(client, bufnr)
          if client.supports_method 'textDocument/documentSymbol' then
            require('nvim-navic').attach(client, bufnr)
          end
        end,
      },
    },
    {
      'williamboman/mason-lspconfig.nvim', -- MUST be set up before `nvim-lspconfig`
      dependencies = {
        {
          'williamboman/mason.nvim',
          cmd = { 'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUpdate' },
          opts_extend = { 'ensure_installed' },
          opts = {
            ---@type '"prepend"' | '"append"' | '"skip"'
            PATH = 'append',
            ui = {
              icons = {
                package_pending = ' ',
                package_installed = '󰄳 ',
                package_uninstalled = ' 󰚌',
              },
            },
            ensure_installed = {
              'astro-language-server',
              'bash-language-server',
              'clangd',
              'css-lsp',
              'docker-compose-language-service',
              'dockerfile-language-server',
              'elixir-ls',
              'eslint-lsp',
              'eslint_d',
              'gofumpt',
              'goimports',
              'golangci-lint',
              'gopls',
              'html-lsp',
              'htmlhint',
              'json-lsp',
              'lua-language-server',
              'ols',
              'prettierd',
              'prisma-language-server',
              'pyright',
              'ruff',
              'shellcheck',
              'shfmt',
              'stylua',
              'svelte-language-server',
              'tailwindcss-language-server',
              'typescript-language-server',
              'vtsls',
              'vue-language-server',
              'yaml-language-server',
              'yamllint',
              'zk',
            },
          },
          config = function(_, opts)
            require('mason').setup(opts)
            local mr = require 'mason-registry'
            mr.refresh(function()
              for _, tool in ipairs(opts.ensure_installed or {}) do
                local p = mr.get_package(tool)
                if not p:is_installed() then
                  p:install()
                end
              end
            end)
          end,
        },
      },
      opts = {
        -- use AstroLSP setup for mason-lspconfig
        handlers = {
          function(server)
            require('astrolsp').lsp_setup(server)
          end,
        },
      },
    },
  },
  config = function()
    local diags = { Error = '', Warn = '', Info = '', Hint = '' }
    for sign, icon in pairs(diags) do
      vim.fn.sign_define('DiagnosticSign' .. sign, { text = icon, texthl = 'DiagnosticSign' .. sign })
    end
    -- set up servers configured with AstroLSP
    vim.tbl_map(require('astrolsp').lsp_setup, require('astrolsp').config.servers)
    vim.g.rustaceanvim = {
      server = {
        on_attach = function(client, bufnr)
          -- you can also put keymaps in here
          require('astrolsp').on_attach(client, bufnr)
        end,
      },
    }
  end,
}
