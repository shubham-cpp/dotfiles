local config = {
  default_theme = {
    plugins = {
      hop = true,
    },
  },
  lsp = {
    mappings = {
      n = {
        ['<leader>='] = {
          function()
            vim.lsp.buf.format({ async = false })
          end,
          desc = 'Format File',
        },
        ['<leader>ff'] = {
          '<cmd>Telescope find_files hidden=true<cr>',
          desc = 'Find Files(hidden)',
        },
        ['<C-\\>'] = { '<Cmd>exe v:count1 . "ToggleTerm"<CR>', desc = 'Open terminal' },
      },
      i = {
        ['<C-\\>'] = { '<Esc><Cmd>exe v:count1 . "ToggleTerm"<CR>', desc = 'Open terminal' },
      },
      t = {
        ['<C-l>'] = false,
        ['<C-k>'] = false,
      },
    },
  },
  mappings = {
    n = {
      ['<C-\\>'] = false,
      ['<leader>c'] = false,
      ['0'] = { '^' },
      ['<Esc>'] = { '<cmd>nohl<cr>', desc = 'No Highlight' },
      [',w'] = { '<cmd>w!<cr>', desc = 'Save File' },
      [',W'] = { '<cmd>noautocmd w!<cr>', desc = 'Save File(Without Aus)' },
      ['<A-h>'] = { '<cmd>tabp<cr>', desc = 'Switch to Prev Tab' },
      ['<A-l>'] = { '<cmd>tabn<cr>', desc = 'Switch to Next Tab' },
      ['<A-j>'] = { 'mz:m+<cr>`z', desc = 'Move line down' },
      ['<A-k>'] = { 'mz:m-2<cr>`z', desc = 'Move line up' },
      ['<C-p>'] = { '<cmd>Telescope find_files<cr>', desc = 'Find Files' },
      ['<C-n>'] = { '<cmd>Neotree focus toggle<cr>' },
      ['<A-/>'] = { '"ayy"apk<Plug>(comment_toggle_linewise_current)j', desc = 'Copy Line and Comment' },
      ['dl'] = { '"_dl' },
      ['c'] = { '"_c' },
      ['C'] = { '"_C' },
      ['k'] = { "v:count == 0 ? 'gk' : 'k'", expr = true, silent = true },
      ['j'] = { "v:count == 0 ? 'gj' : 'j'", expr = true, silent = true },
    },
    v = {
      p = {
        [[ 'pgv"'.v:register.'y' ]],
        expr = true,
        noremap = true,
        silent = false,
        desc = "Don't Copy when pasting in visual mode",
      },
      c = { '"_c' },
      J = { ":m '>+1<CR>gv=gv", desc = 'Move Line Down' },
      K = { ":m '<-2<CR>gv=gv", desc = 'Move Line Up' },
    },
    i = {
      [','] = { ',<C-g>u' },
      ['.'] = { '.<C-g>u' },
      ['?'] = { '?<C-g>u' },
    },
    o = {
      ['ie'] = { ':exec "normal! ggVG"<cr>', desc = 'New operator for entire file' },
      ['iv'] = { ':exec "normal! HVL"<cr>', desc = 'New operator for entire file' },
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
  },
}

return config
