---@type 'luasnip'|'mini_snippets'
local snippet_preset = 'luasnip'
---@type LazySpec
return {
  'saghen/blink.cmp',
  event = 'InsertEnter',
  version = '*',
  dependencies = {
    'mikavilpas/blink-ripgrep.nvim',
    'kristijanhusak/vim-dadbod-completion',
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      dependencies = { 'Bilal2453/luvit-meta' },
      opts = {
        library = {
          'lazy.nvim',
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
        },
      },
    },
    {
      'saghen/blink.compat',
      optional = true, -- make optional so it's only enabled if any extras need it
      opts = {},
    },
    {
      'L3MON4D3/LuaSnip',
      version = 'v2.*',
      enabled = true,
      build = 'make install_jsregexp',
      dependencies = {
        {
          'rafamadriz/friendly-snippets',
          config = function()
            require('luasnip.loaders.from_vscode').lazy_load()
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
      init = function()
        snippet_preset = 'mini_snippets'
      end,
      opts = { history = true, delete_check_events = 'TextChanged' },
    },
    {
      'echasnovski/mini.snippets',
      enabled = false,
      dependencies = { 'rafamadriz/friendly-snippets' },
      init = function()
        snippet_preset = 'mini_snippets'
      end,
      config = function()
        local gen_loader = require('mini.snippets').gen_loader
        require('mini.snippets').setup({
          mappings = { expand = '', jump_next = '', jump_prev = '' },
          snippets = {
            -- Load snippets based on current language by reading files from
            -- "snippets/" subdirectories from 'runtimepath' directories.
            gen_loader.from_lang(),
          },
        })
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
      accept = { auto_brackets = { enabled = true } },
      menu = { border = 'rounded', draw = { treesitter = { 'lsp' } } },
      documentation = { window = { border = 'rounded' }, auto_show = true, auto_show_delay_ms = 200 },
      list = {
        selection = {
          preselect = function(ctx)
            return ctx.mode ~= 'cmdline'
          end,
        },
      },
    },
    sources = {
      default = {
        'lsp',
        'snippets',
        'path',
        'buffer',
        'ripgrep',
        'lazydev',
        'markdown',
        'dadbod',
      },
      providers = {
        dadbod = { name = 'Dadbod', module = 'vim_dadbod_completion.blink' },
        ripgrep = {
          module = 'blink-ripgrep',
          name = 'Ripgrep',
          ---@module "blink-ripgrep"
          ---@type blink-ripgrep.Options
          opts = {
            prefix_min_len = 4,
            score_offset = 10, -- should be lower priority
            max_filesize = '300K',
            search_casing = '--smart-case',
          },
        },
        lazydev = {
          name = 'LazyDev',
          module = 'lazydev.integrations.blink',
        },
        markdown = {
          name = 'RenderMarkdown',
          module = 'render-markdown.integ.blink',
          fallbacks = { 'lsp' },
        },
        lsp = {
          name = 'LSP',
          module = 'blink.cmp.sources.lsp',
          fallbacks = { 'lazydev' },
          score_offset = 150, -- the higher the number, the higher the priority
          -- Filter text items from the LSP provider, since we have the buffer provider for that
          transform_items = function(_, items)
            for _, item in ipairs(items) do
              if item.kind == require('blink.cmp.types').CompletionItemKind.Snippet then
                item.score_offset = item.score_offset - 50
              end
            end

            return vim.tbl_filter(function(item)
              return item.kind ~= require('blink.cmp.types').CompletionItemKind.Text
            end, items)
          end,
        },
        -- snippets = {
        --   name = 'Snippets',
        --   module = 'blink.cmp.sources.snippets',
        --   score_offset = 40,
        -- },
        path = {
          name = 'Path',
          module = 'blink.cmp.sources.path',
          score_offset = 25,
          opts = {
            trailing_slash = false,
            label_trailing_slash = true,
          },
        },
        buffer = {
          name = 'Buffer',
          module = 'blink.cmp.sources.buffer',
          min_keyword_length = 3,
          score_offset = 15,
        },
      },
    },
    snippets = { preset = snippet_preset },
  },
}
