local sp = require 'sp.util'
return {
  {
    'neovim/nvim-lspconfig',
    enabled = false,
    dependencies = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'b0o/schemastore.nvim',
      {
        'lvimuser/lsp-inlayhints.nvim',
        config = function()
          require('lsp-inlayhints').setup()
          vim.api.nvim_create_autocmd('LspAttach', {
            group = sp.au_lsp,
            callback = function(args)
              if not (args.data and args.data.client_id) then
                return
              end

              local bufnr = args.buf
              local client = vim.lsp.get_client_by_id(args.data.client_id)
              require('lsp-inlayhints').on_attach(client, bufnr)
            end,
          })
        end,
      },
      {
        'folke/neodev.nvim',
        opts = {},
      },
      {
        'creativenull/efmls-configs-nvim',
        version = 'v1.x.x',
      },
      -- 'simrat39/rust-tools.nvim'
    },
    config = function()
      local lspconfig = require 'lspconfig'
      require('lspconfig.ui.windows').default_options.border = 'rounded'
      local util = lspconfig.util
      vim.diagnostic.config({
        --- {{{
        underline = true,
        update_in_insert = false,
        virtual_text = {
          spacing = 4,
          source = 'always',
          -- severity = 'error'
          -- prefix = '👾',
        },
        signs = true,
        severity_sort = true,
        float = { show_header = true, source = 'always' },
      })
      vim.fn.sign_define(
        'DiagnosticSignError',
        { text = require('sp.icons').diagnostics.BoldError, texthl = 'DiagnosticSignError' }
      )
      vim.fn.sign_define(
        'DiagnosticSignWarn',
        { text = require('sp.icons').diagnostics.BoldWarning, texthl = 'DiagnosticSignWarn' }
      )
      vim.fn.sign_define(
        'DiagnosticSignInfo',
        { text = require('sp.icons').diagnostics.BoldInformation, texthl = 'DiagnosticSignInfo' }
      )
      vim.fn.sign_define(
        'DiagnosticSignHint',
        { text = require('sp.icons').diagnostics.BoldHint, texthl = 'DiagnosticSignHint' }
      )
      local cmp_capabilities = vim.lsp.protocol.make_client_capabilities()
      cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
      cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
        properties = { 'documentation', 'detail', 'additionalTextEdits' },
      }
      cmp_capabilities = require('cmp_nvim_lsp').default_capabilities(cmp_capabilities)
      cmp_capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }

      local mason_lspconfig = require 'mason-lspconfig'
      require('mason').setup({
        ---@type '"prepend"' | '"append"' | '"skip"'
        PATH = 'append',
        ui = {
          icons = {
            package_installed = '✓',
            package_uninstalled = '✗',
            package_pending = '⟳',
          },
        },
      })
      mason_lspconfig.setup({
        ensure_installed = {
          'lua_ls',
          'efm',
          'astro',
          'bashls',
          'clangd',
          'cssls',
          'docker_compose_language_service',
          'dockerls',
          'efm',
          'emmet_language_server',
          'eslint',
          'gopls',
          'html',
          'jsonls',
          'prismals',
          'svelte',
          'tailwindcss',
          'tsserver',
          'vimls',
          'volar',
          'yamlls',
          'zk',
        },
      })
      local opts = {}
      opts.capabilities = cmp_capabilities
      opts.on_attach = sp.on_attach
      -- opts.flags = { debounce_text_changes = 150, }
      mason_lspconfig.setup_handlers({
        emmet_ls = function()
          lspconfig['emmet_ls'].setup(opts)
        end,
        clangd = function()
          local opt = vim.deepcopy(opts)
          opt.capabilities.offsetEncoding = 'utf-8'
          opt.cmd = {
            'clangd',
            '--background-index',
            '--clang-tidy',
            '--suggest-missing-includes',
            '--header-insertion=iwyu',
            '--completion-style=detailed',
            '--function-arg-placeholders',
            '--pch-storage=memory',
            '--fallback-style=llvm',
          }
          opt.init_options = {
            usePlaceholders = true,
            completeUnimported = true,
            clangdFileStatus = true,
          }
          lspconfig.clangd.setup(opt)
        end,
        efm = function()
          -- local eslint = require 'efmls-configs.linters.eslint_d'
          local prettier = require 'efmls-configs.formatters.prettier_d'
          local stylua = require 'efmls-configs.formatters.stylua'
          local shfmt = require 'efmls-configs.formatters.shfmt'
          local shellcheck = require 'efmls-configs.linters.shellcheck'
          local go = {
            require 'efmls-configs.linters.golangci_lint',
            require 'efmls-configs.formatters.gofumpt',
          }
          local c = {
            require 'efmls-configs.formatters.clang_format',
            require 'efmls-configs.linters.clang_tidy',
          }

          local languages = {
            c = c,
            cpp = c,
            go = go,
            typescript = { prettier },
            typescriptreact = { prettier },
            javascript = { prettier },
            javascriptreact = { prettier },
            html = { prettier },
            css = { prettier },
            scss = { prettier },
            less = { prettier },
            graphql = { prettier },
            json = { prettier },
            jsonc = { prettier },
            json5 = { prettier },
            vue = { prettier },
            svelte = { prettier },
            markdown = { prettier },
            lua = { stylua },
            bash = { shfmt, shellcheck },
            sh = { shfmt, shellcheck },
            zsh = { shfmt, shellcheck },
          }
          lspconfig.efm.setup({
            init_options = {
              documentFormatting = true,
              documentRangeFormatting = true,
            },
            filetypes = vim.tbl_keys(languages),
            settings = {
              rootMarkers = { '.git/' },
              languages = languages,
            },
            -- cmd = {
            --   'efm-langserver',
            --   -- '-c',
            --   -- vim.fn.expand '~/.config/efm-langserver/config.yaml',
            --   '-logfile',
            --   '/tmp/efm-langserver-logs.log',
            -- },
            on_attach = function(_, buf)
              vim.keymap.set({ 'n', 'v' }, '<leader>=', function()
                vim.lsp.buf.format({
                  async = true,
                  filter = function(client)
                    return client.name == 'efm'
                  end,
                })
              end, { buffer = buf, desc = 'Efm Fix' })
            end,
          })
        end,
        rust_analyzer = function()
          local opt = vim.deepcopy(opts)
          local function handler(err)
            if err then
              error(tostring(err))
            end
            vim.notify 'Cargo workspace reloaded'
          end
          local function fly_check()
            local params = vim.lsp.util.make_text_document_params()
            vim.lsp.buf_notify(0, 'rust-analyzer/runFlyCheck', params)
          end
          local function reload_workspace()
            vim.notify 'Reloading Cargo Workspace'
            vim.lsp.buf_request(0, 'rust-analyzer/reloadWorkspace', nil, handler)
          end
          opt.on_attach = function(client, bufnr)
            sp.on_attach(client, bufnr)
            vim.keymap.set('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr, desc = 'Format Buffer(LSP)' })
          end
          opt.settings = {
            ['rust-analyzer'] = {
              diagnostics = {
                enable = true,
              },
              check = { features = 'all' },
              inlayHints = {
                closureCaptureHints = {
                  enable = true,
                },
              },
              lens = {
                references = {
                  adt = { enable = true },
                  enumVariant = { enable = true },
                  method = { enable = true },
                  trait = { enable = true },
                },
              },
              typing = { autoClosingAngleBrackets = { enable = true } },
            },
          }
          opt.checkOnSave = {
            allFeatures = true,
            overrideCommand = {
              'cargo',
              'clippy',
              '--workspace',
              '--message-format=json',
              '--all-targets',
              '--all-features',
            },
          }
          opt.commands = {
            RustFlyCheck = { fly_check },
            RustReloadWorkspace = { reload_workspace },
          }
          lspconfig['rust_analyzer'].setup(opt)
        end,
        gopls = function()
          local opt = vim.deepcopy(opts)
          opt.settings = {
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
              codelenses = {
                usePlaceholders = true,
              },
            },
          }
          lspconfig['gopls'].setup(opt)
        end,
        bashls = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/bash-language-server', 'start' }
          opt.settings = { bashIde = { highlightParsingErrors = true } }
          lspconfig['bashls'].setup(opt)
        end,
        html = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/vscode-html-language-server', '--stdio' }
          lspconfig['html'].setup(opt)
        end,
        cssls = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/vscode-css-language-server', '--stdio' }
          opt.settings = {
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
          }
          lspconfig['cssls'].setup(opt)
        end,
        eslint = function()
          lspconfig['eslint'].setup({
            cmd = { sp.bun_path() .. '/vscode-eslint-language-server', '--stdio' },
            on_attach = function(_, bufnr)
              -- vim.api.nvim_create_autocmd('BufWritePre', {
              --   buffer = bufnr,
              --   command = 'EslintFixAll',
              -- })
              vim.keymap.set('n', 'g=', '<cmd>EslintFixAll<CR>', { buffer = bufnr, desc = 'Eslint Fix' })
            end,
          })
        end,
        pylyzer = function()
          local opt = vim.deepcopy(opts)
          lspconfig['pylyzer'].setup(opt)
        end,
        pylsp = function()
          local opt = vim.deepcopy(opts)
          opt.settings = {
            pylsp = {
              plugins = {
                -- formatter options
                black = { enabled = true },
                autopep8 = { enabled = false },
                yapf = { enabled = false },
                flake8 = { enabled = true },
                pycodestyle = { enabled = false },
                -- type checker
                pylsp_mypy = { enabled = true },
                -- auto-completion options
                jedi_completion = { fuzzy = true },
                -- import sorting
                pyls_isort = { enabled = true },
                -- rope_autoimport = { enabled = true, memory = true },
                -- rope_completion = { enabled = true },
              },
            },
          }
          opt.flags = {
            debounce_text_changes = 200,
          }
          lspconfig['pylsp'].setup(opt)
        end,
        pyright = function()
          local opt = vim.deepcopy(opts)
          -- opt.cmd = { sp.bun_path() .. '/pyright-langserver', '--stdio' }
          opt.settings = {
            python = {
              analysis = {
                autoImportCompletions = true,
                typeCheckingMode = 'basic',
              },
              exclude = { '**/node_modules', '**/__pycache__' },
            },
          }
          lspconfig['pyright'].setup(opt)
        end,
        jsonls = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/vscode-json-language-server', '--stdio' }
          opt.settings = {
            json = {
              schemas = vim.list_extend({
                {
                  name = 'Vsnip snippets',
                  description = 'Extend vs code snippet completion for vsnip',
                  fileMatch = { string.format('%s/vsnip/*.json', vim.fn.stdpath 'config') },
                  url = 'https://raw.githubusercontent.com/Yash-Singh1/vscode-snippets-json-schema/main/schema.json',
                },
              }, require('schemastore').json.schemas()),
              validate = { enable = true },
              format = { enable = false },
            },
          }
          lspconfig['jsonls'].setup(opt)
        end,
        yamlls = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/yaml-language-server', '--stdio' }
          opt.settings = {
            redhat = { telemetry = { enabled = false } },
            yaml = {
              schemas = require('schemastore').yaml.schemas({}),
            },
          }
          lspconfig['yamlls'].setup(opt)
        end,
        ['lua_ls'] = function()
          -- IMPORTANT: make sure to setup neodev BEFORE lspconfig
          require('neodev').setup({
            -- add any options here, or leave empty to use the default settings
          })
          local opt = vim.deepcopy(opts)
          -- local runtime_path = vim.split(package.path, ';')
          -- table.insert(runtime_path, 'lua/?.lua')
          -- table.insert(runtime_path, 'lua/?/init.lua')
          -- table.insert(runtime_path, '/usr/share/awesome/lib/?.lua')
          -- table.insert(runtime_path, '/usr/share/awesome/lib/?/?.lua')
          -- table.insert(runtime_path, '/usr/share/awesome/themes/?/?.lua')
          -- table.insert(runtime_path, '/usr/share/awesome/lib/awful/?.lua')
          -- table.insert(runtime_path, '/usr/share/awesome/lib/wibox/?.lua')
          opt.settings = {
            Lua = {
              diagnostics = {
                enable = true,
                globals = { 'vim', 'describe' },
                disable = { 'lowercase-global' },
              },
              workspace = { checkThirdParty = false },
              -- runtime = { version = 'LuaJIT', path = runtime_path },
              runtime = { version = 'LuaJIT' },
              telemetry = { enable = false },
              hint = {
                enable = true,
                setType = true,
                semicolon = 'SameLine',
                paramType = true,
                await = true,
                arrayIndex = 'Enable',
              },
            },
          }
          lspconfig.lua_ls.setup(opt)
        end,
        tailwindcss = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/tailwindcss-language-server', '--stdio' }
          opt.root_dir = util.root_pattern(
            'tailwind.config.js',
            'tailwind.config.cjs',
            'tailwind.config.mjs',
            'tailwind.config.ts',
            'postcss.config.js',
            'postcss.config.cjs',
            'postcss.config.mjs',
            'postcss.config.ts'
          )
          opt.settings = {
            tailwindCSS = {
              emmetCompletions = true,
              validate = 'error',
            },
          }
          opt.single_file_support = false
          lspconfig['tailwindcss'].setup(opt)
        end,
        tsserver = function()
          -- local opt = vim.deepcopy(opts)
          -- opt.cmd = { sp.bun_path() .. '/typescript-language-server', '--stdio' }
          -- opt.filetypes = { 'javascriptreact', 'typescriptreact', 'javascript.jsx', 'typescript.tsx' }
          -- opt.root_dir = util.root_pattern('package.json', 'tsconfig.json', 'jsconfig.json')
          -- opt.init_options = {
          --   hostInfo = 'neovim',
          --   plugins = {
          --     -- for TypeScript v4.9+
          --     -- '@styled/typescript-styled-plugin',
          --     -- or for older TypeScript versions
          --     'typescript-styled-plugin',
          --   },
          -- }
          -- opt.settings = {
          --   javascript = {
          --     inlayHints = {
          --       includeInlayEnumMemberValueHints = true,
          --       includeInlayFunctionLikeReturnTypeHints = true,
          --       includeInlayFunctionParameterTypeHints = true,
          --       includeInlayParameterNameHints = 'all', -- 'none' | 'literals' | 'all';
          --       includeInlayParameterNameHintsWhenArgumentMatchesName = true,
          --       includeInlayPropertyDeclarationTypeHints = true,
          --       includeInlayVariableTypeHints = true,
          --     },
          --   },
          --   typescript = {
          --     inlayHints = {
          --       includeInlayEnumMemberValueHints = true,
          --       includeInlayFunctionLikeReturnTypeHints = true,
          --       includeInlayFunctionParameterTypeHints = true,
          --       includeInlayParameterNameHints = 'all', -- 'none' | 'literals' | 'all';
          --       includeInlayParameterNameHintsWhenArgumentMatchesName = true,
          --       includeInlayPropertyDeclarationTypeHints = true,
          --       includeInlayVariableTypeHints = true,
          --     },
          --   },
          -- }
          -- require('lspconfig').tsserver.setup(opt)
        end,
        denols = function()
          local opt = vim.deepcopy(opts)
          opt.root_dir = util.root_pattern('deno.json', 'deno.jsonc')
          opt.settings = {
            deno = {
              enable = true,
              lint = true,
              codeLens = {
                references = true,
                test = true,
                implementations = true,
                referencesAllFunctions = false,
              },
              inlayHints = {
                enumMemberValues = { enabled = true },
                variableTypes = { enabled = true, suppressWhenArgumentMatchesName = true },
                parameterNames = { enabled = 'all', suppressWhenArgumentMatchesName = true },
                parameterTypes = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
              },
              suggest = {
                completeFunctionCalls = true,
                autoImports = true,
                imports = {
                  hosts = {
                    ['https://crux.land'] = true,
                    ['https://deno.land'] = true,
                    ['https://x.nest.land'] = true,
                  },
                },
              },
            },
          }

          lspconfig['denols'].setup(opt)
        end,
        volar = function()
          local function get_typescript_server_path(root_dir)
            -- local global_ts = vim.fn.stdpath 'data' .. '/mason/packages/vue-language-server/node_modules/typescript/lib'
            local global_ts = string.format('%s/install/global/node_modules/typescript/lib', os.getenv 'BUN_INSTALL')
            local found_ts = ''
            local function check_dir(path)
              found_ts = util.path.join(path, 'node_modules', 'typescript', 'lib')
              if util.path.exists(found_ts) then
                return path
              end
            end
            if util.search_ancestors(root_dir, check_dir) then
              return found_ts
            else
              return global_ts
            end
          end
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/vue-language-server', '--stdio' }
          -- opt.filetypes = { 'typescript', 'javascript', 'vue' }
          opt.filetypes = { 'vue' }
          opt.on_new_config = function(new_config, new_root_dir)
            new_config.init_options.typescript.tsdk = get_typescript_server_path(new_root_dir)
          end
          lspconfig['volar'].setup(opt)
        end,
        svelte = function()
          local opt = vim.deepcopy(opts)
          opt.cmd = { sp.bun_path() .. '/svelteserver', '--stdio' }
          opt.settings = {
            svelte = {
              ['enable-ts-plugin'] = true,
              plugin = { svelte = { defaultScriptLanguage = 'typescript' } },
            },
          }
          lspconfig['svelte'].setup(opt)
        end,
        function(server_name)
          if server_name ~= 'zk' then
            lspconfig[server_name].setup(opts)
          end
        end,
      })
      lspconfig['pylsp'].setup({
        capabilities = cmp_capabilities,
        on_attach = sp.on_attach,
        settings = {
          pylsp = {
            plugins = {
              -- formatter options
              black = { enabled = true },
              autopep8 = { enabled = false },
              yapf = { enabled = false },
              flake8 = { enabled = true },
              pycodestyle = { enabled = false },
              -- type checker
              pylsp_mypy = { enabled = true },
              -- auto-completion options
              jedi_completion = { fuzzy = true },
              -- import sorting
              pyls_isort = { enabled = true },
              -- rope_autoimport = { enabled = true, memory = true },
              -- rope_completion = { enabled = true },
            },
          },
        },
        flags = {
          debounce_text_changes = 200,
        },
      })

      lspconfig['ocamllsp'].setup({
        on_attach = function(client, bufnr)
          sp.on_attach(client, bufnr)
          vim.keymap.set(
            'n',
            '<leader>=',
            '<cmd>lua vim.lsp.buf.format{async=true}<CR>',
            { buffer = bufnr, desc = 'LspFormat' }
          )
        end,
        capabilities = cmp_capabilities,
      })
    end,
  },
  {
    'pmizio/typescript-tools.nvim',
    dependencies = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
    opts = {
      on_attach = function(client, buffer)
        sp.on_attach(client, buffer)
        vim.keymap.set('n', 'go', '<cmd>TSToolsOrganizeImports<cr>', { buffer = buffer })
        vim.keymap.set('n', 'gD', '<cmd>TSToolsGoToSourceDefinition<cr>', { buffer = buffer })
        vim.keymap.set('n', 'gR', '<cmd>TSToolsFileReferences<cr>', { buffer = buffer })
        vim.keymap.set('n', '<F2>', '<cmd>TSToolsRenameFile<space>', { buffer = buffer, silent = false })
      end,
      settings = {
        tsserver_file_preferences = {
          includeInlayParameterNameHints = 'literals',
          includeCompletionsForModuleExports = true,
          includeCompletionsForImportStatements = true,
          includeAutomaticOptionalChainCompletions = true,
          includeCompletionsWithClassMemberSnippets = true,
          allowIncompleteCompletions = true,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayVariableTypeHints = true,
          includeInlayVariableTypeHintsWhenTypeMatchesName = false,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeCompletionsWithSnippetText = true,
        },
        tsserver_plugins = {
          '@styled/typescript-styled-plugin',
        },
        tsserver_max_memory = '3072',
      },
    },
    ft = { 'javascriptreact', 'typescriptreact', 'javascript.jsx', 'typescript.tsx', 'javascript', 'typescript' },
    config = function()
      require('typescript-tools').setup({
        on_attach = function(client, bufnr)
          sp.on_attach(client, bufnr)
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
        },
      })
    end,
  },
}
