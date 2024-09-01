---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'phpcbf',
        'phpstan',
        'phpcs',
        'php-cs-fixer',
        'blade-formatter',
        'phpactor',
        'php-debug-adapter',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      local parser_config = require('nvim-treesitter.parsers').get_parser_configs()

      parser_config.blade = {
        install_info = {
          url = 'https://github.com/EmranMR/tree-sitter-blade',
          files = { 'src/parser.c' },
          branch = 'main',
        },
        filetype = 'blade',
      }

      vim.filetype.add({
        pattern = {
          ['.*%.blade%.php'] = 'blade',
        },
      })
      opts.ensure_installed = require('plugins.config.util').unique_append_table(opts.ensure_installed, {
        'php',
        'phpdoc',
        'blade',
      })
      return opts
    end,
  },
  {
    'ricardoramirezr/blade-nav.nvim',
    dependencies = {
      'hrsh7th/nvim-cmp', -- if using nvim-cmp
    },
    ft = { 'blade', 'php' },
  },
  {
    -- Add the Laravel.nvim plugin which gives the ability to run Artisan commands
    -- from Neovim.
    'adalessa/laravel.nvim',
    enabled = false,
    dependencies = {
      -- "nvim-telescope/telescope.nvim",
      'tpope/vim-dotenv',
      'MunifTanjim/nui.nvim',
      -- "nvimtools/none-ls.nvim",
    },
    cmd = { 'Sail', 'Artisan', 'Composer', 'Npm', 'Yarn', 'Laravel' },
    keys = {
      { '<leader>La', ':Laravel artisan<cr>' },
      { '<leader>Lr', ':Laravel routes<cr>' },
      { '<leader>Lm', ':Laravel related<cr>' },
    },
    ft = { 'blade', 'php' },
    opts = {
      ---@type 'phpactor'|'intelephense'
      lsp_server = 'phpactor',
      features = { null_ls = { enable = false } },
    },
  },
  {
    'AstroNvim/astrolsp',
    dependencies = {
      {
        'gbprod/phpactor.nvim',
        enabled = false,
        dependencies = {
          'nvim-lua/plenary.nvim',
        },
        ft = 'php',
        build = function()
          require 'phpactor.handler.update'()
        end,
      },
    },
    opts = {
      config = {
        blade = {
          cmd = { 'laravel-dev-tools', 'lsp' },
          filetypes = { 'blade' },
          root_dir = function(fname)
            return require('lspconfig').util.find_git_ancestor(fname)
          end,
        },
      },
      handlers = {
        phpactor = false,
        -- phpactor = function(_, opts)
        --   require('phpactor').setup({
        --     install = {
        --       -- path = os.getenv('HOME') .. '',
        --       bin = vim.fn.expand '~/Downloads/GitClones/phpactor/bin/phpactor',
        --     },
        --     lspconfig = {
        --       enabled = true,
        --       options = {
        --         -- capabilities = opts.capabilities,
        --         on_attach = opts.on_attach,
        --         init_options = {
        --           ['language_server_phpstan.enabled'] = true,
        --           ['phpunit.enabled'] = true,
        --         },
        --       },
        --     },
        --   })
        -- end,
        -- intelephense = false,
        blade = false,
      },
    },
  },
}
