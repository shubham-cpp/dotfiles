return {
  'neovim/nvim-lspconfig',
  event = { 'BufReadPost', 'BufNewFile' },
  dependencies = {
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'b0o/schemastore.nvim',
    -- 'jose-elias-alvarez/typescript.nvim',
    {
      'pmizio/typescript-tools.nvim',
      dependencies = {
        "nvim-lua/plenary.nvim",
      }
    },
    'lvimuser/lsp-inlayhints.nvim',
    -- 'simrat39/rust-tools.nvim'
  },
  config = function()
    local lspconfig = require 'lspconfig'
    require("lspconfig.ui.windows").default_options.border = "rounded"
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
    vim.fn.sign_define('DiagnosticSignError', { text = ' ', texthl = 'DiagnosticSignError' })
    vim.fn.sign_define('DiagnosticSignWarn', { text = ' ', texthl = 'DiagnosticSignWarn' })
    vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
    vim.fn.sign_define('DiagnosticSignHint', { text = ' ', texthl = 'DiagnosticSignHint' })
    local function map(mode, lhs, rhs, opts) -- {{{
      opts.buffer = opts.buffer == nil and true or opts.buffer
      opts.noremap = opts.noremap == nil and true or opts.noremap
      opts.silent = opts.silent == nil and true or opts.silent
      vim.keymap.set(mode, lhs, rhs, opts)
    end --- }}}

    local function lsp_organize_imports()
      vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' } }, apply = true })
      vim.lsp.buf.code_action({ context = { only = { 'source.removeUnused' } }, apply = true })
    end
    local au_lsp = vim.api.nvim_create_augroup('sp_lsp', { clear = true })

    local function on_attach(client, bufnr) -- {{{
      local ok_fzf, _ = pcall(require, 'fzf-lua')
      vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
      map('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr, desc = 'Goto Definition' })
      map('n', 'gD', vim.lsp.buf.declaration, { buffer = bufnr, desc = 'Goto Declaration' })
      map('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
      map('i', '<C-h>', vim.lsp.buf.signature_help, { buffer = bufnr })
      map('n', 'gi', vim.lsp.buf.implementation, { buffer = bufnr, desc = 'Goto Implementation' })
      map('n', 'gs', lsp_organize_imports, { buffer = bufnr, desc = 'Organize Imports' })
      if not ok_fzf then
        map('n', 'gr', vim.lsp.buf.references, { buffer = bufnr, desc = 'Lsp References' })
        map('n', 'gw', vim.lsp.buf.document_symbol, { buffer = bufnr, desc = 'Document Symbols' })
        map('n', 'gW', vim.lsp.buf.workspace_symbol, { buffer = bufnr, desc = 'Workspace Symbols' })
      end
      map('n', 'gt', vim.lsp.buf.type_definition, { buffer = bufnr, desc = 'Type Definition' })
      map('n', 'gac', vim.lsp.buf.code_action, { buffer = bufnr, desc = 'Code Actions' })
      map('n', '<F2>', vim.lsp.buf.rename, { buffer = bufnr, desc = 'Lsp Rename' })
      map('n', '<leader>la', vim.lsp.buf.code_action, { buffer = bufnr, desc = 'Lsp Code Actions' })
      map('n', '<leader>lr', vim.lsp.buf.rename, { buffer = bufnr, desc = 'Lsp Rename' })
      -- map('n', '<Space>=', vim.lsp.buf.format, { buffer = bufnr,desc="Format Buffer(LSP)" })
      map('n', 'gl', vim.diagnostic.open_float, { buffer = bufnr, desc = 'diagnostic open' })
      map('n', '[d', vim.diagnostic.goto_prev, { buffer = bufnr, desc = 'Goto Prev Diagnostic' })
      map('n', ']d', vim.diagnostic.goto_next, { buffer = bufnr, desc = 'Goto Next Diagnostic' })
      map('n', '[e', function()
        vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR })
      end, { buffer = bufnr, desc = 'Goto Prev Error' })
      map('n', ']e', function()
        vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR })
      end, { buffer = bufnr, desc = 'Goto Next Error' })

      if client.name == 'prismals' then
        map('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr, desc = 'Format Buffer(LSP)' })
      end

      if client.name == 'svelte' then
        vim.api.nvim_create_autocmd('BufWritePost', {
          pattern = { '*.js', '*.ts' },
          group = au_lsp,
          callback = function(ctx)
            client.notify('$/onDidChangeTsOrJsFile', { uri = ctx.file })
          end,
        })
      end
      -- vim.api.nvim_create_autocmd({ 'BufWrite' }, {
      --   pattern = { '+page.server.ts', '+page.ts', '+layout.server.ts', '+layout.ts' },
      --   group = au_lsp,
      --   command = 'LspRestart svelte',
      -- })
      if client.server_capabilities.documentHighlightProvider then
        vim.api.nvim_exec(
          [[
		        hi LspReferenceWrite cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
		        hi LspReferenceRead cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
		        hi LspReferenceText cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
		        hi LspDiagnosticsDefaultError cterm=bold ctermbg=red guifg=#ff3333
		        hi LspDiagnosticsDefaultWarning cterm=bold ctermbg=red guifg=#e7ae05
		        augroup lsp_document_highlight
		        autocmd! * <buffer>
		        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
		        " autocmd CursorHold <buffer> lua vim.diagnostic.open_float(0, {scope='line'})
		        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
		        " autocmd CursorHoldI <buffer> silent! lua vim.lsp.buf.signature_help()
		        augroup END
		    ]],
          false
        )
      end
      require('lsp-inlayhints').on_attach(client, bufnr)
    end -- }}}

    local cmp_capabilities = vim.lsp.protocol.make_client_capabilities()
    cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
    cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
      properties = { 'documentation', 'detail', 'additionalTextEdits' },
    }
    cmp_capabilities.textDocument.foldingRange = {
      dynamicRegistration = false,
      lineFoldingOnly = true,
    }
    cmp_capabilities = require('cmp_nvim_lsp').default_capabilities(cmp_capabilities)

    local mason_lspconfig = require 'mason-lspconfig'
    require('mason').setup({
      ui = {
        icons = {
          package_installed = '✓',
          package_uninstalled = '✗',
          package_pending = '⟳',
        },
      },
    })
    mason_lspconfig.setup({
      ensure_installed = { 'lua_ls', 'gopls' },
    })
    local opts = {}
    opts.capabilities = cmp_capabilities
    opts.on_attach = on_attach
    -- opts.flags = { debounce_text_changes = 150, }
    mason_lspconfig.setup_handlers({
      function(server_name)
        if server_name ~= 'zk' then
          lspconfig[server_name].setup(opts)
        end
      end,
      emmet_ls = function()
        lspconfig.emmet_ls.setup(opts)
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
        lspconfig.gopls.setup(opt)
      end,
      bashls = function()
        local opt = vim.deepcopy(opts)
        -- opt.cmd = { require('sp.util').bun_path() .. '/bash-language-server', 'start' }
        opt.settings = { bashIde = { highlightParsingErrors = true } }
        lspconfig.bashls.setup(opt)
      end,
      html = function()
        local opt = vim.deepcopy(opts)
        -- opt.cmd = { require('sp.util').bun_path() .. '/vscode-html-language-server', '--stdio' }
        lspconfig.html.setup(opt)
      end,
      cssls = function()
        local opt = vim.deepcopy(opts)
        -- opt.cmd = { require('sp.util').bun_path() .. '/vscode-css-language-server', '--stdio' }
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
        lspconfig.cssls.setup(opt)
      end,
      eslint = function()
        lspconfig.eslint.setup({
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
        lspconfig.pylyzer.setup(opt)
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
        lspconfig.pylsp.setup(opt)
      end,
      pyright = function()
        local opt = vim.deepcopy(opts)
        -- opt.cmd = { require('sp.util').bun_path() .. '/pyright-langserver', '--stdio' }
        opt.settings = {
          python = {
            analysis = {
              autoImportCompletions = true,
              typeCheckingMode = 'basic',
            },
            exclude = { '**/node_modules', '**/__pycache__' },
          },
        }
        lspconfig.pyright.setup(opt)
      end,
      jsonls = function()
        local opt = vim.deepcopy(opts)
        -- opt.cmd = { require('sp.util').bun_path() .. '/vscode-json-language-server', '--stdio' }
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
        lspconfig.jsonls.setup(opt)
      end,
      yamlls = function()
        local opt = vim.deepcopy(opts)
        -- opt.cmd = { require('sp.util').bun_path() .. '/yaml-language-server', '--stdio' }
        opt.settings = {
          redhat = { telemetry = { enabled = false } },
          yaml = {
            schemas = require('schemastore').json.schemas({
              select = {
                'docker-compose.yml',
                '.yarnrc.yml',
                'GitHub Workflow',
                'GitHub Workflow Template Properties',
                'GitHub Action',
                'Azure Pipelines',
              },
            }),
          },
        }
        lspconfig.yamlls.setup(opt)
      end,
      ['lua_ls'] = function()
        local opt = vim.deepcopy(opts)
        local runtime_path = vim.split(package.path, ';')
        table.insert(runtime_path, 'lua/?.lua')
        table.insert(runtime_path, 'lua/?/init.lua')
        table.insert(runtime_path, '/usr/share/awesome/lib/?.lua')
        table.insert(runtime_path, '/usr/share/awesome/lib/?/?.lua')
        table.insert(runtime_path, '/usr/share/awesome/themes/?/?.lua')
        -- table.insert(runtime_path, '/usr/share/awesome/lib/awful/?.lua')
        -- table.insert(runtime_path, '/usr/share/awesome/lib/wibox/?.lua')
        opt.settings = {
          Lua = {
            diagnostics = {
              enable = true,
              globals = { 'vim', 'describe' },
              disable = { 'lowercase-global' },
            },
            runtime = { version = 'LuaJIT', path = runtime_path },
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
        -- opt.cmd = { require('sp.util').bun_path() .. '/tailwindcss-language-server', '--stdio' }
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
        -- opt.single_file_support = false
        lspconfig.tailwindcss.setup(opt)
      end,
      tsserver = function()
        require("typescript-tools").setup {
          on_attach = on_attach,
          root_dir = util.root_pattern('package.json', 'tsconfig.json', 'jsconfig.json'),
          filetypes = { 'javascriptreact', 'typescriptreact', 'javascript.jsx', 'typescript.tsx' },
          settings = {
            tsserver_file_preferences = {
              includeInlayParameterNameHints = "literals",
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
          }
        }
        -- require('typescript').setup({
        --   disable_commands = false, -- prevent the plugin from creating Vim commands
        --   debug = false,            -- enable debug logging for commands
        --   go_to_source_definition = { fallback = true },
        --   server = {
        --     cmd = { require('sp.util').bun_path() .. '/typescript-language-server', '--stdio' },
        --     filetypes = { 'javascriptreact', 'typescriptreact', 'javascript.jsx', 'typescript.tsx' },
        --     root_dir = util.root_pattern('package.json', 'tsconfig.json', 'jsconfig.json'),
        --     on_attach = on_attach,
        --     capabilities = cmp_capabilities,
        --     settings = {
        --       javascript = {
        --         inlayHints = {
        --           includeInlayEnumMemberValueHints = true,
        --           includeInlayFunctionLikeReturnTypeHints = true,
        --           includeInlayFunctionParameterTypeHints = true,
        --           includeInlayParameterNameHints = 'all', -- 'none' | 'literals' | 'all';
        --           includeInlayParameterNameHintsWhenArgumentMatchesName = true,
        --           includeInlayPropertyDeclarationTypeHints = true,
        --           includeInlayVariableTypeHints = true,
        --         },
        --       },
        --       typescript = {
        --         inlayHints = {
        --           includeInlayEnumMemberValueHints = true,
        --           includeInlayFunctionLikeReturnTypeHints = true,
        --           includeInlayFunctionParameterTypeHints = true,
        --           includeInlayParameterNameHints = 'all', -- 'none' | 'literals' | 'all';
        --           includeInlayParameterNameHintsWhenArgumentMatchesName = true,
        --           includeInlayPropertyDeclarationTypeHints = true,
        --           includeInlayVariableTypeHints = true,
        --         },
        --       },
        --     },
        --   },
      -- })
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

        lspconfig.denols.setup(opt)
      end,
      volar = function()
        local function get_typescript_server_path(root_dir)
          -- local global_ts = vim.fn.stdpath 'data' .. '/mason/packages/vue-language-server/node_modules/typescript/lib'
          local global_ts = string.format("%s/install/global/node_modules/typescript/lib", os.getenv("BUN_INSTALL"))
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
        opt.cmd = { require('sp.util').bun_path() .. '/vue-language-server', '--stdio' }
        opt.filetypes = { 'typescript', 'javascript', 'vue' }
        opt.on_new_config = function(new_config, new_root_dir)
          new_config.init_options.typescript.tsdk = get_typescript_server_path(new_root_dir)
        end
        opt.settings = {
          vue = {
            inlayHints = {
              inlineHandlerLeading = true,
              missingProps = true,
              optionsWrapper = true,
            },
            updateImportsOnFileMove = { enabled = true },
          },
        }
        lspconfig.volar.setup(opt)
      end,
      svelte = function()
        local opt = vim.deepcopy(opts)
        opt.cmd = { require('sp.util').bun_path() .. '/svelteserver', '--stdio' }
        opt.settings = {
          svelte = {
            ['enable-ts-plugin'] = true,
            plugin = { svelte = { defaultScriptLanguage = 'typescript' } },
          },
        }
        lspconfig.svelte.setup(opt)
      end,
      efm = function()
        lspconfig.efm.setup({
          settings = {
            documentFormatting = true,
            documentRangeFormatting = true,
            hover = false,
            documentSymbol = false,
            codeAction = false,
            completion = false
          },
          filetypes = {
            'javascript',
            'javascriptreact',
            'typescript',
            'typescriptreact',
            'json',
            'jsonc',
            'html',
            'css',
            'scss',
            'less',
            'go',
            'vue',
            'svelte',
            'graphql',
            'yaml',
            'sh',
            'bash',
            'fish',
            'c',
            'cpp',
            'vim',
            'python',
            'lua',
            'markdown'
          },
          on_attach = function(_, buf)
            vim.keymap.set(
              'n',
              '<leader>=',
              '<cmd>lua vim.lsp.buf.format{async=true}<CR>',
              { buffer = buf, desc = 'Efm Fix' }
            )
          end,
        })
      end,
    })
    lspconfig.nim_langserver.setup({
      on_attach = on_attach,
      capabilities = cmp_capabilities,
    })

    lspconfig.pylsp.setup({
      capabilities = cmp_capabilities,
      on_attach = on_attach,
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

    -- lspconfig.nimls.setup({
    --   on_attach = on_attach,
    --   capabilities = cmp_capabilities,
    -- })
  end,
}
