local util = require 'user.lsp.util'

local config = {
  lsp = {
    config = {
      html = { cmd = { util.bun_path() .. '/vscode-html-language-server', '--stdio' } },
      cssls = { cmd = { util.bun_path() .. '/vscode-css-language-server', '--stdio' } },
      jsonls = { cmd = { util.bun_path() .. '/vscode-json-language-server', '--stdio' } },
      eslint = { cmd = { util.bun_path() .. '/vscode-eslint-language-server', '--stdio' } },
      volar = { cmd = { util.bun_path() .. '/vue-language-server', '--stdio' } },
      bashls = { cmd = { util.bun_path() .. '/bash-language-server', 'start' } },
      awk_ls = { cmd = { util.bun_path() .. '/awk-language-server' } },
      docker_compose_language_service = { cmd = { util.bun_path() .. '/docker-compose-langserver', '--stdio' } },
      dockerls = { cmd = { util.bun_path() .. '/docker-langserver', '--stdio' } },
      tailwindcss = { cmd = { util.bun_path() .. '/tailwindcss-language-server', '--stdio' } },
      svelte = { cmd = { util.bun_path() .. '/svelteserver', '--stdio' } },
      emmet_ls = { cmd = { util.bun_path() .. '/emmet-ls', '--stdio' } },
      vimls = { cmd = { util.bun_path() .. '/vim-language-server', '--stdio' } },
      astro = { cmd = { util.bun_path() .. '/astro-ls', '--stdio' } },
      prismals = { cmd = { util.bun_path() .. '/prisma-language-server', '--stdio' } },
    },
  },
  plugins = {
    {
      'L3MON4D3/LuaSnip',
      config = function(plugin, opts)
        require 'plugins.configs.luasnip' (plugin, opts) -- include the default astronvim config that calls the setup call
        require('luasnip.loaders.from_vscode').lazy_load({
          paths = { '~/Documents/dotfiles/.config/astronvim/lua/user/snippets' },
        }) -- load snippets paths
      end,
    },
    {
      'akinsho/nvim-toggleterm.lua',
      version = '*',
      config = function(_, opts)
        opts.open_mapping = [[<c-\>]]
        return opts
      end,
    },
    {
      'hrsh7th/nvim-cmp',
      dependencies = {
        'hrsh7th/cmp-nvim-lsp-signature-help',
        'hrsh7th/cmp-cmdline',
        'hrsh7th/cmp-nvim-lua',
      },
      config = function(_, opts)
        local cmp = require 'cmp'
        local sources = require 'cmp.config.sources'
        opts.mapping['<C-x><C-x>'] = cmp.mapping.complete({
          config = { sources = { { name = 'luasnip' } } },
        })
        opts.mapping['<C-x><C-f>'] = cmp.mapping.complete({
          config = { sources = { { name = 'path' } } },
        })
        opts.mapping['<CR>'] = cmp.mapping({
          i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
          c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
        })

        opts.sources = cmp.config.sources({

          { name = 'nvim_lsp',                priority = 1000 },
          { name = 'nvim_lua',                priority = 1000 },
          { name = 'nvim_lsp_signature_help', priority = 900 },
          { name = 'luasnip',                 priority = 750 },
          {
            name = 'buffer',
            priority = 300,
            option = {
              get_bufnrs = function()
                return vim.api.nvim_list_bufs()
              end,
            },
          },
          { name = 'path', priority = 250 },
        })

        cmp.setup.cmdline(':', {
          sources = sources({
            {
              name = 'cmdline',
              option = {
                ignore_cmds = { 'Man', '!' },
              },
            },
            { name = 'path' },
          }),
          mapping = cmp.mapping.preset.cmdline({}),
        })

        cmp.setup.cmdline('/', {
          sources = sources({
            { name = 'nvim_lsp_signature_help' },
            { name = 'buffer',                 keyword_pattern = [=[[^[:blank:]].*]=] },
          }),
          mapping = cmp.mapping.preset.cmdline({}),
        })
        return opts
      end,
    },
    {
      'nvim-neo-tree/neo-tree.nvim',
      version = '*',
      config = function(_, opts)
        -- opts.default_component_configs.name.trailing_slash = true
        opts.window.mappings['l'] = 'open'
        opts.window.mappings['t'] = 'open_tabnew'
        opts.window.mappings['?'] = 'show_help'
        opts.window.mappings['<'] = 'prev_source'
        opts.window.mappings['>'] = 'next_source'
        return opts
      end,
    },
    {
      'nvim-treesitter/nvim-treesitter',
      version = false,
      dependencies = {
        'nvim-treesitter/nvim-treesitter-textobjects',
      },
      config = function(_, opts)
        opts.textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ['af'] = '@function.outer',
              ['if'] = '@function.inner',
              ['ac'] = '@class.outer',
              ['ic'] = '@class.inner',
            },
          },
          swap = {
            enable = true,
            swap_next = {
              [']z'] = '@parameter.inner',
            },
            swap_previous = {
              ['[z'] = '@parameter.inner',
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              [']m'] = '@function.outer',
              [']]'] = '@class.outer',
            },
            goto_next_end = {
              [']M'] = '@function.outer',
              [']['] = '@class.outer',
            },
            goto_previous_start = {
              ['[m'] = '@function.outer',
              ['[['] = '@class.outer',
            },
            goto_previous_end = {
              ['[M'] = '@function.outer',
              ['[]'] = '@class.outer',
            },
          },
        }
        return opts
      end,
    },
    {
      'jose-elias-alvarez/null-ls.nvim',
      config = function(_, opts)
        local null_ls = require 'null-ls'
        local my_sources = {
          null_ls.builtins.diagnostics.mypy,
        }
        -- table.insert(opts.sources, null_ls.builtins.diagnostics.mypy)
        print(vim.inspect(opts))
        opts.sources = vim.tbl_extend('keep', opts.sources or {}, my_sources)
        return opts
      end,
    },
  },
}

return config
