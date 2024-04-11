return {
  'neovim/nvim-lspconfig',
  event = 'BufReadPost',
  dependencies = {
    {
      'AstroNvim/astrolsp',
      dependencies = {
        {
          'creativenull/efmls-configs-nvim',
          version = 'v1.x.x',
          enabled = false,
        },
        {
          'folke/neodev.nvim',
          opts = {},
        },
        'b0o/schemastore.nvim',
        { 'pmizio/typescript-tools.nvim', dependencies = 'nvim-lua/plenary.nvim' },
        { 'SmiteshP/nvim-navic', enabled = false, opts = {} },
      },
      ---@type AstroLSPConfig
      opts = {
        -- Configuration table of features provided by AstroLSP
        features = {
          autoformat = true, -- enable or disable auto formatting on start
          codelens = true, -- enable/disable codelens refresh on start
          inlay_hints = true, -- enable/disable inlay hints on start
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
                    -- Depending on the usage, you might want to add additional paths here.
                    -- "${3rd}/luv/library"
                    -- "${3rd}/busted/library",
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
            setting = {
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
              -- "--offset-encoding=utf-16",
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
                vim.lsp.buf.code_action({ context = { only = { 'source.removeUnused' } }, apply = true })
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
            -- ["<leader>="] = {
            -- 	vim.lsp.buf.formatting,
            -- 	desc = "Format buffer(lsp)",
            -- 	cond = "textDocument/formatting",
            -- },
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
              cond = function(client, bufnr)
                return client.server_capabilities.semanticTokensProvider and vim.lsp.semantic_tokens
              end,
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
          tsserver = function(_, opts)
            require('typescript-tools').setup({
              on_attach = function(client, bufnr)
                -- if client.server_capabilities.documentSymbolProvider then
                -- 	vim.b[bufnr].navic_enabled = true
                -- 	require("nvim-navic").attach(client, bufnr)
                -- end
                opts.on_attach(client, bufnr)
                vim.keymap.set('n', 'go', '<cmd>TSToolsOrganizeImports<cr>', { buffer = bufnr })
                vim.keymap.set('n', 'gD', '<cmd>TSToolsGoToSourceDefinition<cr>', { buffer = bufnr })
                vim.keymap.set('n', 'gR', '<cmd>TSToolsFileReferences<cr>', { buffer = bufnr })
                vim.keymap.set('n', '<F2>', '<cmd>TSToolsRenameFile<space>', { buffer = bufnr, silent = false })
              end,
              settings = {
                tsserver_file_preferences = {
                  includeInlayParameterNameHints = 'all',
                  includeCompletionsForModuleExports = true,
                  includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                  includeInlayFunctionParameterTypeHints = true,
                  includeInlayVariableTypeHints = true,
                  includeInlayVariableTypeHintsWhenTypeMatchesName = false,
                  includeInlayPropertyDeclarationTypeHints = true,
                  includeInlayFunctionLikeReturnTypeHints = true,
                  includeInlayEnumMemberValueHints = true,
                },
                tsserver_plugins = {
                  -- for TypeScript v4.9+
                  '@styled/typescript-styled-plugin',
                  -- or for older TypeScript versions
                  -- "typescript-styled-plugin",
                },
                tsserver_max_memory = '3072',
              },
            })
          end,
          lua_ls = function(server, opts)
            require('neodev').setup({
              -- add any options here, or leave empty to use the default settings
            })
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)

            require('lspconfig')[server].setup(opts)
          end,
          jsonls = function(server, opts)
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)
            opts.settings = {
              schemas = require('schemastore').json.schemas({}),
              validate = { enable = true },
              format = { enable = false },
            }
            -- local on = opts.on_attach
            -- opts.on_attach = function(client, bufnr)
            -- 	if client.server_capabilities.documentSymbolProvider then
            -- 		vim.b[bufnr].navic_enabled = true
            -- 		require("nvim-navic").attach(client, bufnr)
            -- 	end
            -- 	on(client, bufnr)
            -- end
            require('lspconfig')[server].setup(opts)
          end,
          yamlls = function(server, opts)
            -- opts.capabilities = require("cmp_nvim_lsp").default_capabilities(opts.capabilities)
            opts.capabilities = require('cmp_nvim_lsp').default_capabilities()
            opts.settings = {
              redhat = { telemetry = { enabled = false } },
              yaml = {
                validate = true,
                schemas = require('schemastore').yaml.schemas({}),
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
          eslint = false,
          efm = false,
          -- efm = function()
          -- 	local prettier = require("efmls-configs.formatters.prettier_d")
          -- 	local stylua = require("efmls-configs.formatters.stylua")
          -- 	local go = {
          -- 		require("efmls-configs.linters.golangci_lint"),
          -- 		require("efmls-configs.formatters.gofumpt"),
          -- 	}
          -- 	local c = {
          -- 		require("efmls-configs.formatters.clang_format"),
          -- 		require("efmls-configs.linters.clang_tidy"),
          -- 	}
          -- 	local python = {
          -- 		require("efmls-configs.formatters.isort"),
          -- 		require("efmls-configs.formatters.ruff"),
          -- 		require("efmls-configs.linters.ruff"),
          -- 	}
          -- 	local sh = {
          -- 		require("efmls-configs.formatters.shfmt"),
          -- 		require("efmls-configs.linters.shellcheck"),
          -- 	}
          -- 	local languages = {
          -- 		bash = sh,
          -- 		c = c,
          -- 		cpp = c,
          -- 		css = { prettier },
          -- 		go = go,
          -- 		graphql = { prettier },
          -- 		html = { prettier },
          -- 		javascript = { prettier },
          -- 		javascriptreact = { prettier },
          -- 		json = { prettier },
          -- 		json5 = { prettier },
          -- 		jsonc = { prettier },
          -- 		less = { prettier },
          -- 		lua = { stylua },
          -- 		markdown = { prettier },
          -- 		python = python,
          -- 		sass = { prettier },
          -- 		scss = { prettier },
          -- 		sh = sh,
          -- 		svelte = { prettier },
          -- 		typescript = { prettier },
          -- 		typescriptreact = { prettier },
          -- 		vue = { prettier },
          -- 	}
          -- 	require("lspconfig").efm.setup({
          -- 		init_options = {
          -- 			documentFormatting = true,
          -- 			documentRangeFormatting = true,
          -- 		},
          -- 		filetypes = vim.tbl_keys(languages),
          -- 		settings = {
          -- 			rootMarkers = { ".git/" },
          -- 			languages = languages,
          -- 		},
          --
          -- 		on_attach = function(_, buf)
          -- 			vim.keymap.set({ "n", "v" }, "<leader>=", function()
          -- 				vim.lsp.buf.format({
          -- 					async = true,
          -- 					filter = function(client)
          -- 						return client.name == "efm"
          -- 					end,
          -- 				})
          -- 			end, { buffer = buf, desc = "Efm Fix" })
          -- 		end,
          -- 	})
          -- end,
          zk = false,
        },
        -- A list like table of servers that should be setup, useful for enabling language servers not installed with Mason.
        servers = { 'gleam', 'zls' },
      },
    },
    {
      'williamboman/mason-lspconfig.nvim', -- MUST be set up before `nvim-lspconfig`
      dependencies = {
        {
          'williamboman/mason.nvim',
          opts = {
            ---@type '"prepend"' | '"append"' | '"skip"'
            PATH = 'append',

            ui = {
              icons = {
                package_installed = '✓',
                package_uninstalled = '✗',
                package_pending = '⟳',
              },
            },
          },
        },
      },
      opts = function()
        return {
          ensure_installed = {
            'astro',
            'bashls',
            'clangd',
            'cssls',
            'docker_compose_language_service',
            'dockerls',
            'emmet_language_server',
            -- "eslint",
            -- "efm",
            'gopls',
            'html',
            'jsonls',
            'lua_ls',
            'prismals',
            'pyright',
            'svelte',
            'tailwindcss',
            'tsserver',
            'volar',
            'yamlls',
            'zk',
          },
          -- use AstroLSP setup for mason-lspconfig
          handlers = {
            function(server)
              require('astrolsp').lsp_setup(server)
            end,
          },
        }
      end,
    },
  },
  config = function()
    local diags = { Error = '', Warn = '', Info = '', Hint = '' }
    for sign, icon in pairs(diags) do
      vim.fn.sign_define('DiagnosticSign' .. sign, { text = icon, texthl = 'DiagnosticSign' .. sign })
    end
    -- vim.fn.sign_define(
    --   'DiagnosticSignError',
    --   { text = '', texthl = 'DiagnosticSignError' }
    -- )
    -- vim.fn.sign_define(
    --   'DiagnosticSignWarn',
    --   { text = '', texthl = 'DiagnosticSignWarn' }
    -- )
    -- vim.fn.sign_define(
    --   'DiagnosticSignInfo',
    --   { text = '', texthl = 'DiagnosticSignInfo' }
    -- )
    -- vim.fn.sign_define(
    --   'DiagnosticSignHint',
    --   { text = '', texthl = 'DiagnosticSignHint' }
    -- )
    -- set up servers configured with AstroLSP
    vim.tbl_map(require('astrolsp').lsp_setup, require('astrolsp').config.servers)
  end,
}
