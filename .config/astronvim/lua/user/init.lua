local util = require 'user.lsp.util'

local config = {
  default_theme = {
    plugins = {
      hop = false,
    },
  },
  lsp = {
    ['server-settings'] = {
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
    toggleterm = function(config_tt)
      config_tt.open_mapping = '<F1>'
      -- config_tt.open_mapping = "<C-'>"
      return config_tt
    end,
    ['hrsh7th/cmp-buffer'] = {
      config = function()
        astronvim.add_user_cmp_source({
          name = 'buffer',
          option = {
            keyword_length = 2,
            get_bufnrs = function()
              return vim.api.nvim_list_bufs()
            end,
          },
        })
      end,
    },
    cmp = function(opts)
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

      cmp.setup.cmdline(':', {
        sources = sources({
          { name = 'cmdline', option = {
            ignore_cmds = { 'Man', '!' },
          } },
          { name = 'path' },
        }),
        mapping = cmp.mapping.preset.cmdline({}),
      })

      cmp.setup.cmdline('/', {
        sources = sources({
          { name = 'nvim_lsp_signature_help' },
          { name = 'buffer', keyword_pattern = [=[[^[:blank:]].*]=] },
        }),
        mapping = cmp.mapping.preset.cmdline({}),
      })
      return opts
    end,
    ['neo-tree'] = function(opts)
      -- opts.default_component_configs.name.trailing_slash = true
      opts.window.mappings['l'] = 'open'
      opts.window.mappings['t'] = 'open_tabnew'
      opts.window.mappings['?'] = 'show_help'
      opts.window.mappings['<'] = 'prev_source'
      opts.window.mappings['>'] = 'next_source'
      return opts
    end,
    treesitter = function(opts)
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
    ['null-ls'] = function(opts)
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
  luasnip = {
    vscode = {
      paths = {
        '~/Documents/dotfiles/.config/astronvim/lua/user/snippets',
      },
    },
  },
}

return config
