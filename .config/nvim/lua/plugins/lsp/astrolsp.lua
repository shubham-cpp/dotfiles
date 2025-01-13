---@type LazySpec
return {
  'AstroNvim/astrolsp',
  lazy = true,
  dependencies = {
    'AstroNvim/astrocore',
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
    'williamboman/mason-lspconfig.nvim',
    -- 'hrsh7th/nvim-cmp',
    {
      'iguanacucumber/magazine.nvim',
      name = 'nvim-cmp', -- Otherwise highlighting gets messed up
    },
  },
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
    },
    commands = {
      Format = {
        function()
          vim.lsp.buf.format({ async = true })
        end,
        cond = 'textDocument/formatting',
        desc = 'Format file with LSP',
      },
      ImportsOrganize = {
        function()
          vim.lsp.buf.code_action({
            context = { only = { 'source.organizeImports' } },
            apply = true,
          })
        end,
        cond = 'textDocument/codeAction',
        desc = 'Organize Imports',
      },
      ImportsRemove = {
        function()
          vim.lsp.buf.code_action({
            context = { only = { 'source.removeUnused' } },
            apply = true,
          })
        end,
        cond = 'textDocument/codeAction',
        desc = 'Remove Unused Imports',
      },
      FixAll = {
        function()
          vim.lsp.buf.code_action({
            context = { only = { 'source.fixAll' } },
            apply = true,
          })
        end,
        cond = 'textDocument/codeAction',
        desc = 'Fix All fixable diagnostics',
      },
    },
    capabilities = {
      textDocument = { foldingRange = { dynamicRegistration = false, lineFoldingOnly = true } },
      workspace = {
        didChangeWatchedFiles = vim.fn.has 'nvim-0.10' == 0 and { dynamicRegistration = true },
      },
    },
    config = {
      bashls = {
        settings = { bashIde = { highlightParsingErrors = true } },
      },
    },
    mappings = {
      n = {
        K = { vim.lsp.buf.hover, desc = 'Hover', cond = 'textDocument/hover' },
        gl = { vim.diagnostic.open_float, desc = 'Hover diagnostics' },
        ['<leader>le'] = { vim.diagnostic.open_float, desc = 'Hover diagnostics' },
        ['<leader>ld'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.definition()
            else
              fzf.lsp_definitions({
                jump_to_single_result = true,
                winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
              })
            end
          end,
          desc = 'Goto definition',
          cond = 'textDocument/definition',
        },
        gd = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.definition()
            else
              fzf.lsp_definitions({
                jump_to_single_result = true,
                winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
              })
            end
          end,
          desc = 'Goto definition',
          cond = 'textDocument/definition',
        },
        ['<leader>lf'] = {
          function()
            vim.lsp.buf.code_action({
              context = { only = { 'source.fixAll' } },
              apply = true,
            })
          end,
          desc = 'Fix All',
          cond = 'textDocument/codeAction',
        },
        ['<leader>lo'] = {
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
        ['<leader>li'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.implementation()
            else
              fzf.lsp_implementations()
            end
          end,
          desc = 'Goto implementation',
          cond = 'textDocument/implementation',
        },
        -- condition for only server with declaration capabilities
        ['<leader>lD'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.declaration()
            else
              fzf.lsp_declarations({
                jump_to_single_result = true,
                winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
              })
            end
          end,
          desc = 'Goto Declaration',
          cond = 'textDocument/declaration',
        },
        ['<leader>lt'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.type_definition()
            else
              fzf.lsp_typedefs()
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
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.references()
            else
              fzf.lsp_references({
                jump_to_single_result = true,
                winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
              })
            end
          end,
          desc = 'Goto references',
          cond = 'textDocument/references',
        },
        ['<leader>lR'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.references()
            else
              fzf.lsp_references({
                jump_to_single_result = true,
                winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
              })
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
        [']d'] = {
          vim.diagnostic.goto_next,
          desc = 'Goto Next diagnostic',
        },
        ['[d'] = {
          vim.diagnostic.goto_prev,
          desc = 'Goto Next diagnostic',
        },
        ['<leader>lw'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.document_symbol()
            else
              fzf.lsp_document_symbols()
            end
          end,
          desc = 'Document symbols',
          cond = 'textDocument/documentSymbol',
        },
        ['<leader>lW'] = {
          function()
            local ok, fzf = pcall(require, 'fzf-lua')
            if not ok then
              vim.lsp.buf.workspace_symbol()
            else
              fzf.lsp_workspace_symbols()
            end
          end,
          desc = 'Workspace symbols',
          cond = 'workspace/symbol',
        },
        -- ['<F2>'] = {
        --   vim.lsp.buf.rename,
        --   desc = 'Rename',
        --   cond = 'textDocument/rename',
        -- },
        ['<leader>lr'] = {
          vim.lsp.buf.rename,
          desc = 'Rename',
          cond = 'textDocument/rename',
        },
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
          desc = 'Toggle LSP inlay highlight (buffer)',
          cond = function(client)
            return client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint
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
        opts.capabilities = require('plugins.config.util').get_lsp_capabilities(opts.capabilities)
        require('lspconfig')[server].setup(opts)
      end,
      efm = false,
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
}
