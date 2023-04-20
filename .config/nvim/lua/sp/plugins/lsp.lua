return {
  'neovim/nvim-lspconfig',
  after = 'nvim-cmp',
  dependencies = {
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'b0o/schemastore.nvim',
    'jose-elias-alvarez/typescript.nvim',
    'lvimuser/lsp-inlayhints.nvim',
  },
  config = function()
    local lspconfig = require 'lspconfig'
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
    vim.fn.sign_define('DiagnosticSignError', { text = ' ', texthl = 'DiagnosticSignError' })
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

    local function on_attach(client, bufnr) -- {{{
      map('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr })
      map('n', 'gD', vim.lsp.buf.declaration, { buffer = bufnr })
      map('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
      map('i', '<C-h>', vim.lsp.buf.signature_help, { buffer = bufnr })
      map('n', 'gi', vim.lsp.buf.implementation, { buffer = bufnr })
      map('n', 'gs', lsp_organize_imports, { buffer = bufnr })
      map('n', 'gr', vim.lsp.buf.references, { buffer = bufnr })
      map('n', 'gt', vim.lsp.buf.type_definition, { buffer = bufnr })
      map('n', 'gw', vim.lsp.buf.document_symbol, { buffer = bufnr })
      map('n', 'gW', vim.lsp.buf.workspace_symbol, { buffer = bufnr })
      map('n', 'gac', vim.lsp.buf.code_action, { buffer = bufnr })
      map('n', '<F2>', vim.lsp.buf.rename, { buffer = bufnr })
      map('n', '<Space>=', vim.lsp.buf.format, { buffer = bufnr })
      map('n', 'gl', vim.diagnostic.open_float, { buffer = bufnr })
      map('n', '[d', vim.diagnostic.goto_prev, { buffer = bufnr })
      map('n', ']d', vim.diagnostic.goto_next, { buffer = bufnr })
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
      ensure_installed = { 'lua_ls', 'rust_analyzer', 'gopls', 'golangci_lint_ls' },
    })
    mason_lspconfig.setup_handlers({
      function(server_name)
        local settings = {}
        if server_name == 'jsonls' then
          settings = {
            json = {
              schemas = vim.list_extend({
                {
                  name = 'Vsnip snippets',
                  description = 'Extend vs code snippet completion for vsnip',
                  fileMatch = { string.format('%s/vsnip/*.json', vim.fn.stdpath 'config') },
                  url = 'https://raw.githubusercontent.com/Yash-Singh1/vscode-snippets-json-schema/main/schema.json',
                },
              }, require('schemastore').json.schemas()),
            },
          }
        elseif server_name == 'yamlls' then
          settings = {
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
            redhat = { telemetry = { enabled = false } },
          }
        elseif server_name == 'bashls' then
          settings = {
            bashIde = { highlightParsingErrors = true },
          }
        elseif server_name == 'pyright' then
          settings = {
            python = {
              analysis = {
                autoImportCompletions = true,
                typeCheckingMode = 'basic',
              },
              exclude = { '**/node_modules', '**/__pycache__' },
            },
          }
        elseif server_name == 'gopls' then
          settings = {
            gopls = {
              experimentalPostfixCompletions = true,
              analyses = { unusedparams = true, shadow = true, nilness = true, unusedwrite = true },
              hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                constantValues = true,
                parameterNames = true,
                rangeVariableTypes = true,
              },
              staticcheck = true,
              codelenses = {
                usePlaceholders = true,
              },
            },
          }
        end
        lspconfig[server_name].setup({
          capabilities = cmp_capabilities,
          on_attach = on_attach,
          flags = {
            debounce_text_changes = 150,
          },
          settings,
        })
      end,
      ['lua_ls'] = function()
        local runtime_path = vim.split(package.path, ';')
        table.insert(runtime_path, 'lua/?.lua')
        table.insert(runtime_path, 'lua/?/init.lua')
        lspconfig.lua_ls.setup({
          capabilities = cmp_capabilities,
          on_attach = on_attach,
          flags = { debounce_text_changes = 150 },
          settings = {
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
          },
        })
      end,
      tailwindcss = function()
        lspconfig.tailwindcss.setup({
          cmd = { require('sp.util').bun_path() .. '/tailwindcss-language-server', '--stdio' },
          capabilities = cmp_capabilities,
          on_attach = on_attach,
          flags = { debounce_text_changes = 150 },
          root_dir = util.root_pattern(
            'tailwind.config.js',
            'tailwind.config.cjs',
            'tailwind.config.ts',
            'postcss.config.js',
            'postcss.config.cjs',
            'postcss.config.ts'
          ),
          settings = {
            tailwindCSS = {
              emmetCompletions = true,
            },
          },
          single_file_support = false,
        })
      end,
      tsserver = function()
        require('typescript').setup({
          disable_commands = false, -- prevent the plugin from creating Vim commands
          debug = false, -- enable debug logging for commands
          go_to_source_definition = { fallback = true },
          server = {
            cmd = { require('sp.util').bun_path() .. '/typescript-language-server', '--stdio' },
            root_dir = util.root_pattern('package.json', 'tsconfig.json'),
            on_attach = on_attach,
            capabilities = cmp_capabilities,
            flags = { debounce_text_changes = 150 },
            settings = {
              javascript = {
                inlayHints = {
                  includeInlayEnumMemberValueHints = true,
                  includeInlayFunctionLikeReturnTypeHints = true,
                  includeInlayFunctionParameterTypeHints = true,
                  includeInlayParameterNameHints = 'all', -- 'none' | 'literals' | 'all';
                  includeInlayParameterNameHintsWhenArgumentMatchesName = true,
                  includeInlayPropertyDeclarationTypeHints = true,
                  includeInlayVariableTypeHints = true,
                },
              },
              typescript = {
                inlayHints = {
                  includeInlayEnumMemberValueHints = true,
                  includeInlayFunctionLikeReturnTypeHints = true,
                  includeInlayFunctionParameterTypeHints = true,
                  includeInlayParameterNameHints = 'all', -- 'none' | 'literals' | 'all';
                  includeInlayParameterNameHintsWhenArgumentMatchesName = true,
                  includeInlayPropertyDeclarationTypeHints = true,
                  includeInlayVariableTypeHints = true,
                },
              },
            },
          },
        })
      end,
      eslint = function()
        lspconfig.eslint.setup({
          cmd = { require('sp.util').bun_path() .. '/vscode-eslint-language-server', '--stdio' },
        })
      end,
      denols = function()
        lspconfig.denols.setup({
          capabilities = cmp_capabilities,
          on_attach = on_attach,
          flags = { debounce_text_changes = 150 },
          root_dir = util.root_pattern('deno.json', 'deno.jsonc'),
          settings = {
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
          },
        })
      end,
    })
  end,
}
