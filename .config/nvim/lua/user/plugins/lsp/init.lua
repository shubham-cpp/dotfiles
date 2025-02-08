---@type LazySpec
return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufWinEnter' },
    dependencies = {
      'SmiteshP/nvim-navic',
      {
        'AstroNvim/astrolsp',
        ---@type AstroLSPConfig
        opts = {
          -- Configuration table of features provided by AstroLSP
          features = {
            codelens = true, -- enable/disable codelens refresh on start
            inlay_hints = true, -- enable/disable inlay hints on start
            semantic_tokens = true, -- enable/disable semantic token highlighting
          },
          -- Configure buffer local auto commands to add when attaching a language server
          autocmds = {
            lsp_document_highlight = {
              cond = 'textDocument/documentHighlight',
              {
                event = { 'CursorHold', 'CursorHoldI' },
                desc = 'Document Highlighting',
                callback = function()
                  vim.lsp.buf.document_highlight()
                end,
              },
              {
                event = { 'CursorMoved', 'CursorMovedI', 'BufLeave' },
                desc = 'Document Highlighting Clear',
                callback = function()
                  vim.lsp.buf.clear_references()
                end,
              },
            },
          },
          commands = {
            -- Format = {
            --   function() vim.lsp.buf.format() end,
            --   -- condition to create the user command
            --   -- can either be a string of a client capability or a function of `fun(client, bufnr): boolean`
            --   cond = "textDocument/formatting",
            --   -- the rest of the user command options (:h nvim_create_user_command)
            --   desc = "Format file with LSP",
            -- },
            ImportsOrganize = {
              function()
                vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' } }, apply = true })
              end,
              cond = 'textDocument/codeAction',
              desc = 'Organize Imports',
            },
            ImportsRemove = {
              function()
                vim.lsp.buf.code_action({ context = { only = { 'source.removeUnused' } }, apply = true })
              end,
              cond = 'textDocument/codeAction',
              desc = 'Remove Unused Imports',
            },
            FixAll = {
              function()
                vim.lsp.buf.code_action({ context = { only = { 'source.fixAll' } }, apply = true })
              end,
              cond = 'textDocument/codeAction',
              desc = 'Fix All fixable diagnostics',
            },
          },
          capabilities = {
            textDocument = { foldingRange = { dynamicRegistration = false } },
            workspace = { fileOperations = { didRename = true, willRename = true } },
          },
          mappings = {
            n = {
              gl = {
                function()
                  vim.diagnostic.open_float()
                end,
                desc = 'Hover diagnostics',
              },
              ['<leader>le'] = { vim.diagnostic.open_float, desc = 'Hover diagnostics' },
              ['<leader>ld'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.diagnostics()
                  elseif ok_fzf then
                    fzf.diagnostics()
                  elseif ok_telescope then
                    builtin.diagnostics()
                  elseif ok_mini then
                    mini_pick.pickers.diagnostic()
                  end
                end,
                desc = 'Diagnostics',
              },
              gd = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_definitions()
                  elseif ok_fzf then
                    fzf.lsp_definitions({
                      jump_to_single_result = true,
                      winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
                    })
                  elseif ok_telescope then
                    builtin.lsp_definitions()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'definition' })
                  else
                    vim.lsp.buf.definiton()
                  end
                end,
                desc = 'Goto definition',
                cond = 'textDocument/definition',
              },
              ['<leader>lD'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_definitions()
                  elseif ok_fzf then
                    fzf.lsp_definitions({
                      jump_to_single_result = true,
                      winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
                    })
                  elseif ok_telescope then
                    builtin.lsp_definitions()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'definition' })
                  else
                    vim.lsp.buf.definiton()
                  end
                end,
                desc = 'Goto definition',
                cond = 'textDocument/definition',
              },
              gD = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_declarations()
                  elseif ok_fzf then
                    fzf.lsp_declarations({
                      jump_to_single_result = true,
                      winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
                    })
                  elseif ok_telescope then
                    builtin.lsp_declarations()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'declaration' })
                  else
                    vim.lsp.buf.declaration()
                  end
                end,
                desc = 'Declaration of current symbol',
                cond = 'textDocument/declaration',
              },
              ['<leader>lf'] = {
                function()
                  vim.lsp.buf.code_action({ context = { only = { 'source.fixAll' } }, apply = true })
                end,
                desc = 'Fix All',
                cond = 'textDocument/codeAction',
              },
              ['<leader>lo'] = {
                function()
                  vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' } }, apply = true })
                  vim.lsp.buf.code_action({ context = { only = { 'source.removeUnused' } }, apply = true })
                end,
                desc = 'Oraganize imports',
                cond = 'textDocument/codeAction',
              },
              ['<leader>li'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_implementations()
                  elseif ok_fzf then
                    fzf.lsp_implementations({ winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } } })
                  elseif ok_telescope then
                    builtin.lsp_implementations()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'implementation' })
                  else
                    vim.lsp.buf.implementation()
                  end
                end,
                desc = 'Goto implementation',
                cond = 'textDocument/implementation',
              },
              ['<leader>lt'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_type_definitions()
                  elseif ok_fzf then
                    fzf.lsp_type_definitions({
                      jump_to_single_result = true,
                      winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
                    })
                  elseif ok_telescope then
                    builtin.lsp_type_definitions()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'type_definition' })
                  else
                    vim.lsp.buf.type_definition()
                  end
                end,
                desc = 'Type definition',
                cond = 'textDocument/typeDefinition',
              },
              ['<leader>F'] = {
                function()
                  vim.cmd [[EslintFixAll]]
                end,
                cond = function(client)
                  return client.name == 'eslint'
                end,
              },
              gr = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_references()
                  elseif ok_fzf then
                    fzf.lsp_references({
                      jump_to_single_result = true,
                      winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
                    })
                  elseif ok_telescope then
                    builtin.lsp_references()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'references' })
                  else
                    vim.lsp.buf.references()
                  end
                end,
                desc = 'Goto references',
                cond = 'textDocument/references',
              },
              ['<leader>lR'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_references()
                  elseif ok_fzf then
                    fzf.lsp_references({
                      jump_to_single_result = true,
                      winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
                    })
                  elseif ok_telescope then
                    builtin.lsp_references()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'references' })
                  else
                    vim.lsp.buf.references()
                  end
                end,
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
              [']d'] = { vim.diagnostic.goto_next, desc = 'Goto Next diagnostic' },
              ['[d'] = { vim.diagnostic.goto_prev, desc = 'Goto Next diagnostic' },
              ['<leader>lw'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_symbols()
                  elseif ok_fzf then
                    fzf.lsp_document_symbols()
                  elseif ok_telescope then
                    builtin.lsp_document_symbols()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'document_symbol' })
                  else
                    vim.lsp.buf.document_symbol()
                  end
                end,
                desc = 'Document symbols',
                cond = 'textDocument/documentSymbol',
              },
              ['<leader>lW'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  local ok_telescope, builtin = pcall(require, 'telescope.builtin')
                  local ok_snacks, picker = pcall(require, 'snacks.picker')
                  local ok_mini, mini_pick = pcall(require, 'mini.pick')

                  if ok_snacks then
                    picker.lsp_workspace_symbols()
                  elseif ok_fzf then
                    fzf.lsp_workspace_symbols()
                  elseif ok_telescope then
                    builtin.lsp_dynamic_workspace_symbols()
                  elseif ok_mini then
                    mini_pick.pickers.lsp({ scope = 'workspace_symbol' })
                  else
                    vim.lsp.buf.workspace_symbol()
                  end
                end,
                desc = 'Workspace symbols',
                cond = 'workspace/symbol',
              },
              ['<leader>lr'] = { vim.lsp.buf.rename, desc = 'Rename', cond = 'textDocument/rename' },
              ['<leader>la'] = {
                function()
                  local ok_fzf, fzf = pcall(require, 'fzf-lua')
                  if ok_fzf then
                    fzf.lsp_code_actions()
                  else
                    vim.lsp.buf.code_action()
                  end
                end,
                desc = 'Code actions',
                cond = 'textDocument/codeAction',
              },
              ['<leader>lI'] = {
                function()
                  vim.cmd 'LspInfo'
                end,
                desc = 'LspInfo',
              },
              ['<leader>uY'] = {
                function()
                  require('astrolsp.toggles').buffer_semantic_tokens()
                end,
                desc = 'Toggle LSP semantic highlight (buffer)',
                cond = function(client, bufnr)
                  return client.server_capabilities.semanticTokensProvider and vim.lsp.semantic_tokens
                end,
              },
              ['<leader>uI'] = {
                function()
                  require('astrolsp.toggles').buffer_inlay_hints()
                end,
                desc = 'Toggle LSP inlay highlight (buffer)',
                cond = function(client)
                  return client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint
                end,
              },
            },
            i = {
              ['<C-k>'] = { vim.lsp.buf.signature_help, desc = 'Signature help', cond = 'textDocument/signatureHelp' },
            },
            x = {
              ['<leader>la'] = {
                function()
                  local ok, fzf = pcall(require, 'fzf-lua')
                  if not ok then
                    vim.lsp.buf.code_action()
                  else
                    fzf.lsp_code_actions()
                  end
                end,
                desc = 'Code actions',
                cond = 'textDocument/codeAction',
              },
            },
          },
          handlers = {
            function(server, opts)
              opts.capabilities = require('user.config.util').get_lsp_capabilities(opts.capabilities)
              require('lspconfig')[server].setup(opts)
            end,
            efm = false,
          },
          on_attach = function(client, bufnr)
            local ok_navic, navic = pcall(require, 'nvim-navic')
            if ok_navic and client.supports_method 'textDocument/documentSymbol' then
              navic.attach(client, bufnr)
            end
          end,
        },
      },
      'mason.nvim',
      {
        'williamboman/mason-lspconfig.nvim', -- MUST be set up before `nvim-lspconfig`
        opts = function()
          return {
            handlers = {
              function(server)
                require('astrolsp').lsp_setup(server)
              end,
            },
          }
        end,
      },
    },
  },
  {
    'williamboman/mason.nvim',
    cmd = 'Mason',
    build = ':MasonUpdate',
    opts_extend = { 'ensure_installed' },
    opts = {
      ---@type '"prepend"' | '"append"' | '"skip"'
      PATH = 'append',
      ui = { icons = { package_installed = '✓', package_uninstalled = '✗', package_pending = '⟳' } },
      ensure_installed = { 'stylua', 'shfmt' },
    },
    ---@param opts MasonSettings | {ensure_installed: string[]}
    config = function(_, opts)
      local diags = { Error = '', Warn = '', Info = '', Hint = '' }
      for sign, icon in pairs(diags) do
        vim.fn.sign_define('DiagnosticSign' .. sign, { text = icon, texthl = 'DiagnosticSign' .. sign })
      end

      require('mason').setup(opts)
      local mr = require 'mason-registry'
      mr:on('package:install:success', function()
        vim.defer_fn(function()
          -- trigger FileType event to possibly load this newly installed LSP server
          require('lazy.core.handler.event').trigger({ event = 'FileType', buf = vim.api.nvim_get_current_buf() })
        end, 100)
      end)

      mr.refresh(function()
        for _, tool in ipairs(opts.ensure_installed) do
          local p = mr.get_package(tool)
          if not p:is_installed() then
            p:install()
          end
        end
      end)
    end,
  },
}
