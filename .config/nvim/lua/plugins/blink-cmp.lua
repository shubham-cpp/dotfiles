---@type LazySpec
return {
  'Saghen/blink.cmp',
  enabled = false,
  -- use a release tag to download pre-built binaries
  version = '*',
  -- optional: provides snippets for the snippet source
  dependencies = {
    'rafamadriz/friendly-snippets',
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
            score_offset = 10, -- should be lower priority
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
  },
}
