return {
  {
    'utilyre/barbecue.nvim',
    event = { 'BufReadPost', 'BufNewFile' },
    name = 'barbecue',
    version = '*',
    dependencies = {
      'SmiteshP/nvim-navic',
      'nvim-tree/nvim-web-devicons', -- optional dependency
    },
    -- opts = {
    --   -- configurations go here
    -- },
    config = true,
  },
  {
    'jay-babu/mason-null-ls.nvim',
    event = { 'BufReadPost', 'BufNewFile' },
    dependencies = {
      'williamboman/mason.nvim',
      'jose-elias-alvarez/null-ls.nvim',
    },
    version = '*',
    config = function()
      local null_ls = require 'null-ls'
      local mason_null_ls = require 'mason-null-ls'
      local handlers = {
        function(source_name, methods)
          require 'mason-null-ls.automatic_setup' (source_name, methods)
        end,
        prettierd = function()
          null_ls.register(null_ls.builtins.formatting.prettierd.with({
            disabled_filetypes = { 'markdown' },
            extra_filetypes = { 'svelte' },
          }))
        end,
        -- fixjson = function()
        --   null_ls.register(null_ls.builtins.formatting.fixjson.with({
        --     filetypes = { 'jsonc', 'json' },
        --     extra_args = { '-i', '2' },
        --   }))
        -- end,
        shfmt = function()
          null_ls.register(null_ls.builtins.formatting.shfmt.with({
            extra_args = { '-i', '2', '-ci' },
          }))
        end,
        black = function()
          null_ls.register(null_ls.builtins.formatting.black.with({
            extra_args = { '-l', '80', '--fast' },
          }))
        end,
      }
      mason_null_ls.setup({
        ensure_installed = { 'stylua', 'prettierd', 'jsonlint' },
        handlers = handlers,
      })
      local my_sources = {
        null_ls.builtins.diagnostics.mypy,
        null_ls.builtins.diagnostics.fish,
        null_ls.builtins.formatting.fish_indent,
        null_ls.builtins.formatting.nimpretty,
        null_ls.builtins.completion.spell.with({
          filetypes = { 'markdown', 'html' },
        }),
      }
      null_ls.setup({
        sources = my_sources,
        on_attach = function(_, bufnr)
          vim.keymap.set('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr, desc = 'Format Buffer(null_ls)' })
          vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = '[D]iagnostic [N]ext', buffer = bufnr })
          vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = '[D]iagnostic [P]rev', buffer = bufnr })
          print 'LSP attached (null-ls)'
        end,
      })
    end,
  },
}
