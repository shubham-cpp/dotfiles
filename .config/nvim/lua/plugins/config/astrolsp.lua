local M = {}

M.opts = {
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
    workspace = {
      didChangeWatchedFiles = vim.fn.has 'nvim-0.10' == 0 and { dynamicRegistration = true },
    },
  },
  -- Configure language servers for `lspconfig` (`:h lspconfig-setup`)
  config = {
    bashls = {
      settings = { bashIde = { highlightParsingErrors = true } },
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
  },

  handlers = {
    function(server, opts)
      opts.capabilities = require('cmp_nvim_lsp').default_capabilities(opts.capabilities)
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
}

return M
