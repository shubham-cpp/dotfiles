-- NOTE: Specify the trigger character(s) used for luasnip
local trigger_text = ';'
---@type LazySpec
return {
  'Saghen/blink.cmp',
  enabled = false,
  version = '*',
  dependencies = {
    'mikavilpas/blink-ripgrep.nvim',
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      opts = {
        library = {
          'lazy.nvim',
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
        },
      },
    },
    { 'Bilal2453/luvit-meta', lazy = true },
    {
      'L3MON4D3/LuaSnip',
      version = 'v2.*',
      build = 'make install_jsregexp',
      dependencies = { 'rafamadriz/friendly-snippets' },
      opts = { history = true, delete_check_events = 'TextChanged', region_check_events = 'CursorMoved' },
      config = function(_, opts)
        if opts then
          require('luasnip').config.setup(opts)
        end

        vim.tbl_map(function(type)
          require('luasnip.loaders.from_' .. type).lazy_load()
        end, { 'vscode', 'snipmate', 'lua' })
        require('luasnip.loaders.from_vscode').lazy_load({ paths = { vim.fn.stdpath 'config' .. '/snippets' } })

        local extends = {
          typescript = { 'tsdoc' },
          javascript = { 'jsdoc' },
          lua = { 'luadoc' },
          python = { 'pydoc' },
          rust = { 'rustdoc' },
          cs = { 'csharpdoc' },
          java = { 'javadoc' },
          c = { 'cdoc' },
          cpp = { 'cppdoc' },
          php = { 'phpdoc' },
          kotlin = { 'kdoc' },
          ruby = { 'rdoc' },
          sh = { 'shelldoc' },
        }
        -- friendly-snippets - enable standardized comments snippets
        for ft, snips in pairs(extends) do
          require('luasnip').filetype_extend(ft, snips)
        end
      end,
    },
  },
  ---@module 'blink.cmp'
  ---@type blink.cmp.Config
  opts = {
    keymap = {
      preset = 'enter',
      ['<C-y>'] = { 'select_and_accept', 'fallback' },
      ['<C-k>'] = { 'select_prev', 'fallback' },
      ['<C-j>'] = { 'select_next', 'fallback' },
      ['<Tab>'] = { 'select_next', 'snippet_forward', 'fallback' },
      ['<S-Tab>'] = { 'select_prev', 'snippet_backward', 'fallback' },
      cmdline = {
        preset = 'enter',
        ['<Up>'] = {},
        ['<Down>'] = {},
        ['<C-k>'] = { 'select_prev', 'fallback' },
        ['<C-j>'] = { 'select_next', 'fallback' },
        ['<Tab>'] = { 'select_next', 'fallback' },
        ['<S-Tab>'] = { 'select_prev', 'fallback' },
      },
    },
    completion = {
      ghost_text = { enabled = false },
      menu = { border = 'rounded' },
      documentation = { window = { border = 'rounded' } },
      list = {
        selection = function(ctx)
          return ctx.mode == 'cmdline' and 'auto_insert' or 'preselect'
        end,
      },
    },
    sources = {
      default = {
        'lazydev',
        'lsp',
        'path',
        'snippets',
        'buffer',
        'ripgrep',
      },
      providers = {
        ripgrep = {
          module = 'blink-ripgrep',
          name = 'Ripgrep',
          ---@module "blink-ripgrep"
          ---@type blink-ripgrep.Options
          opts = {
            prefix_min_len = 4,
            score_offset = -3, -- should be lower priority
            max_filesize = '300K',
            search_casing = '--smart-case',
          },
        },
        lazydev = {
          name = 'LazyDev',
          module = 'lazydev.integrations.blink',
          -- make lazydev completions top priority (see `:h blink.cmp`)
          score_offset = 100,
        },
        lsp = {
          name = 'lsp',
          enabled = true,
          module = 'blink.cmp.sources.lsp',
          kind = 'LSP',
          fallbacks = { 'snippets', 'buffer' },
          score_offset = 95, -- the higher the number, the higher the priority
        },
        path = {
          name = 'Path',
          module = 'blink.cmp.sources.path',
          score_offset = 25,
          fallbacks = { 'buffer' },
          opts = {
            trailing_slash = false,
            label_trailing_slash = true,
            get_cwd = function(context)
              return vim.fn.expand(('#%d:p:h'):format(context.bufnr))
            end,
            show_hidden_files_by_default = true,
          },
        },
        buffer = {
          name = 'Buffer',
          enabled = true,
          module = 'blink.cmp.sources.buffer',
          min_keyword_length = 4,
          score_offset = 15, -- the higher the number, the higher the priority
        },
        snippets = {
          name = 'snippets',
          enabled = true,
          min_keyword_length = 2,
          module = 'blink.cmp.sources.snippets',
          score_offset = 80, -- the higher the number, the higher the priority
          -- Only show snippets if I type the trigger_text characters, so
          -- to expand the "bash" snippet, if the trigger_text is ";" I have to
          should_show_items = function()
            local col = vim.api.nvim_win_get_cursor(0)[2]
            local before_cursor = vim.api.nvim_get_current_line():sub(1, col)
            -- NOTE: remember that `trigger_text` is modified at the top of the file
            return before_cursor:match(trigger_text .. '%w*$') ~= nil
          end,
          -- After accepting the completion, delete the trigger_text characters
          -- from the final inserted text
          transform_items = function(_, items)
            local col = vim.api.nvim_win_get_cursor(0)[2]
            local before_cursor = vim.api.nvim_get_current_line():sub(1, col)
            local trigger_pos = before_cursor:find(trigger_text .. '[^' .. trigger_text .. ']*$')
            if trigger_pos then
              for _, item in ipairs(items) do
                item.textEdit = {
                  newText = item.insertText or item.label,
                  range = {
                    start = { line = vim.fn.line '.' - 1, character = trigger_pos - 1 },
                    ['end'] = { line = vim.fn.line '.' - 1, character = col },
                  },
                }
              end
            end
            -- NOTE: After the transformation, I have to reload the luasnip source
            -- Otherwise really crazy shit happens and I spent way too much time
            -- figurig this out
            vim.schedule(function()
              require('blink.cmp').reload 'snippets'
            end)
            return items
          end,
        },
      },
      cmdline = function()
        local type = vim.fn.getcmdtype()
        if type == ':' then
          return { 'path', 'cmdline' }
        end
        if type == '/' or type == '?' then
          return { 'buffer' }
        end
        return {}
      end,
    },
    snippets = {
      preset = 'luasnip',
      -- This comes from the luasnip extra, if you don't add it, won't be able to
      -- jump forward or backward in luasnip snippets
      -- https://www.lazyvim.org/extras/coding/luasnip#blinkcmp-optional
      expand = function(snippet)
        require('luasnip').lsp_expand(snippet)
      end,
      active = function(filter)
        if filter and filter.direction then
          return require('luasnip').jumpable(filter.direction)
        end
        return require('luasnip').in_snippet()
      end,
      jump = function(direction)
        require('luasnip').jump(direction)
      end,
    },
  },
}
