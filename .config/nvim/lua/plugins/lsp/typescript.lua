local augroup = vim.api.nvim_create_augroup('sp_eslint', {})
---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'vtsls',
        'css-lsp',
        'prettierd',
        'eslint_d',
        'eslint-lsp',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'astro',
        'javascript',
        'typescript',
        'jsdoc',
        'tsx',
        'styled',
      })
      return opts
    end,
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
    'AstroNvim/astrolsp',
    opts = {
      handlers = {
        tsserver = false,
        ts_ls = false,
        eslint = function(server, opts)
          local default_attach = opts.on_attach
          opts.on_attach = function(client, bufnr)
            default_attach(client, bufnr)
            vim.api.nvim_create_autocmd('BufWritePre', {
              buffer = bufnr,
              desc = 'Run Eslint fix before save',
              group = augroup,
              callback = function()
                vim.cmd 'EslintFixAll'
              end,
            })
          end
          require('lspconfig')[server].setup(opts)
        end,
        vtsls = function(server, opts)
          require('lspconfig.configs').vtsls = require('vtsls').lspconfig
          local default_attach = opts.on_attach
          opts.capabilities = require('plugins.config.util').get_lsp_capabilities(opts.capabilities)
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
                    name = '@vue/typescript-plugin',
                    location = require('mason-registry').get_package('vue-language-server'):get_install_path()
                      .. '/node_modules/@vue/language-server',
                    languages = { 'vue' },
                    configNamespace = 'typescript',
                    enableForWorkspaceTypeScriptVersions = true,
                  },
                  {
                    name = 'typescript-svelte-plugin',
                    location = require('mason-registry').get_package('svelte-language-server'):get_install_path()
                      .. '/node_modules/typescript-svelte-plugin',
                    enableForWorkspaceTypeScriptVersions = true,
                  },
                },
              },
            },
            typescript = {
              suggest = { completeFunctionCalls = true },
              updateImportsOnFileMove = { enabled = 'always' },
              inlayHints = {
                parameterNames = { enabled = 'all' },
                parameterTypes = { enabled = true },
                variableTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                enumMemberValues = { enabled = true },
              },
            },
          }
          opts.settings.javascript = vim.tbl_deep_extend('force', {}, opts.settings.typescript, {})
          opts.on_attach = function(client, buffer)
            default_attach(client, buffer)
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
              -- require('vtsls').commands.goto_source_definition(buffer)
              vim.cmd 'VtsExec goto_source_definition'
            end, 'Goto Source Definition')

            map('gR', function()
              require('vtsls').commands.file_references(buffer)
            end, 'File References')

            map('go', function()
              require('vtsls').commands.organize_imports(buffer)
            end, 'Organize Imports')
            map('<leader>lo', function()
              require('vtsls').commands.organize_imports(buffer)
              -- require('vtsls').commands.add_missing_imports(buffer)
              -- require('vtsls').commands.remove_unused_imports(buffer)
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
          end
          require('lspconfig')[server].setup(opts)
        end,
      },
    },
  },
  {
    'vuki656/package-info.nvim',
    dependencies = { 'MunifTanjim/nui.nvim' },
    opts = {},
    event = 'BufRead package.json',
  },
  -- {
  --   'dmmulroy/ts-error-translator.nvim',
  --   ft = {
  --     'typescript',
  --     'typescriptreact',
  --     'javascript',
  --     'javascriptreact',
  --   },
  --   opts = {},
  -- },
  {
    'dmmulroy/ts-error-translator.nvim',
    lazy = true,
    dependencies = {
      'AstroNvim/astrolsp',
      ---@param opts AstroLSPOpts
      opts = function(_, opts)
        if not opts.lsp_handlers then
          opts.lsp_handlers = {}
        end
        local event = 'textDocument/publishDiagnostics'
        local orig = opts.lsp_handlers[event] or vim.lsp.handlers[event]
        opts.lsp_handlers[event] = function(err, result, ctx, config)
          local client = vim.lsp.get_client_by_id(ctx.client_id)
          if
            client
            and vim.tbl_contains({
              'astro',
              'svelte',
              'tsserver',
              'typescript-tools',
              'volar',
              'vtsls',
            }, client.name)
          then
            vim.tbl_map(require('ts-error-translator').translate, result.diagnostics)
          end
          orig(err, result, ctx, config)
        end
      end,
    },
    opts = function()
      return { auto_override_publish_diagnostics = not require('astrocore').is_available 'astrolsp' }
    end,
  },
}
