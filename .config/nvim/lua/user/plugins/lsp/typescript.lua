local augroup = vim.api.nvim_create_augroup('sp_eslint', {})

---@type LazySpec
return {
  { 'pmizio/typescript-tools.nvim', dependencies = { 'nvim-lua/plenary.nvim' }, lazy = true, enabled = false },
  {
    'yioneko/nvim-vtsls',
    lazy = true,
    opts = {
      -- automatically trigger renaming of extracted symbol
      refactor_auto_rename = true,
    },
    config = function(_, opts)
      require('vtsls').config(opts)
    end,
  },
  {
    'AstroNvim/astrolsp',
    ---@type AstroLSPConfig
    opts = {
      handlers = {
        tsserver = false, -- old deprecated server
        vtsls = function(server, opts)
          require('lspconfig.configs')[server] = require('vtsls').lspconfig
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
          opts.on_attach = function(client, buffer)
            default_attach(client, buffer)
            local map = function(lhs, rhs, desc)
              vim.keymap.set('n', lhs, rhs, {
                buffer = buffer,
                noremap = true,
                desc = desc,
              })
            end
            map('gD', function()
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
          opts.settings = {
            complete_function_calls = true,
            vtsls = {
              enableMoveToFileCodeAction = true,
              autoUseWorkspaceTsdk = true,
              experimental = {
                -- maxInlayHintLength = 30,
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
              updateImportsOnFileMove = { enabled = 'always' },
              suggest = {
                completeFunctionCalls = true,
              },
              -- inlayHints = {
              --   enumMemberValues = { enabled = true },
              --   functionLikeReturnTypes = { enabled = true },
              --   parameterNames = { enabled = 'literals' },
              --   parameterTypes = { enabled = true },
              --   propertyDeclarationTypes = { enabled = true },
              --   variableTypes = { enabled = false },
              -- },
            },
          }
          -- copy typescript settings to javascript
          opts.settings.javascript =
            vim.tbl_deep_extend('force', {}, opts.settings.typescript, opts.settings.javascript or {})
          require('lspconfig')[server].setup(opts)
        end,
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
        ts_ls = false,
        -- ts_ls = function(_, opts)
        --   require('typescript-tools').setup({
        --     on_attach = opts.on_attach,
        --     expose_as_code_action = 'all',
        --     tsserver_plugins = { '@styled/typescript-styled-plugin' },
        --   })
        -- end,
      },
    },
  },
  {
    'nvim-treesitter',
    opts = { ensure_installed = { 'javascript', 'typescript', 'tsx', 'jsdoc', 'astro', 'styled' } },
  },
  {
    'mason.nvim',
    opts = { ensure_installed = { 'typescript-language-server', 'eslint-lsp', 'eslint_d', 'vtsls' } },
  },
  {
    'dmmulroy/ts-error-translator.nvim',
    lazy = true,
    dependencies = {
      'AstroNvim/astrolsp',
      optional = true,
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
              'ts_ls',
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
  },
}
