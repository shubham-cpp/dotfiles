return {
  {
    'utilyre/barbecue.nvim',
    after = 'nvim-lspconfig',
    name = 'barbecue',
    version = '*',
    dependencies = {
      'SmiteshP/nvim-navic',
      'nvim-tree/nvim-web-devicons', -- optional dependency
    },
    opts = {
      -- configurations go here
    },
  },
  {
    'jose-elias-alvarez/null-ls.nvim',
    after = 'nvim-lspconfig',
    dependencies = {
      'williamboman/mason.nvim',
      'jay-babu/mason-null-ls.nvim',
    },
    version = '*',
    config = function()
      local null_ls = require 'null-ls'
      local mason_null_ls = require 'mason-null-ls'

      mason_null_ls.setup({
        ensure_installed = { 'stylua', 'prettierd', 'fixjson', 'jsonlint' },
      })

      mason_null_ls.setup_handlers({
        function(source_name, methods)
          require 'mason-null-ls.automatic_setup'(source_name, methods)
        end,
        prettierd = function()
          null_ls.register(null_ls.builtins.formatting.prettierd.with({ disabled_filetypes = { 'json', 'markdown' } }))
        end,
        fixjson = function()
          null_ls.register(null_ls.builtins.formatting.fixjson.with({
            filetypes = { 'jsonc', 'json' },
            extra_args = { '-i', '2' },
          }))
        end,
        shfmt = function()
          null_ls.register(null_ls.builtins.formatting.fixjson.with({
            extra_args = { '-i', '2', '-ci' },
          }))
        end,
        black = function()
          null_ls.register(null_ls.builtins.formatting.black.with({
            extra_args = { '-l', '80', '--fast' },
          }))
        end,
      })

      -- will setup any installed and configured sources above
      null_ls.setup({
        on_attach = function(_, bufnr)
          vim.keymap.set('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr })
          vim.keymap.set('n', ']g', vim.diagnostic.goto_next, { buffer = bufnr })
          vim.keymap.set('n', '[g', vim.diagnostic.goto_prev, { buffer = bufnr })
          print 'LSP attached (null-ls)'
        end,
      })
    end,
  },
}
