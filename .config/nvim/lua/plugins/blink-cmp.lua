---@type LazySpec
return {
  'Saghen/blink.cmp',
  enabled = false,
  lazy = false, -- lazy loading handled internally
  -- optional: provides snippets for the snippet source
  dependencies = {
    'rafamadriz/friendly-snippets',
    {
      'folke/lazydev.nvim',
      ft = 'lua', -- only load on lua files
      opts = {
        library = {
          'lazy.nvim',
          { path = 'luvit-meta/library', words = { 'vim%.uv' } },
        },
      },
    },
    { 'Bilal2453/luvit-meta', lazy = true },
  },

  -- use a release tag to download pre-built binaries
  version = 'v0.*',
  -- OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
  -- build = 'cargo build --release',

  opts = {
    keymap = {
      hide = '<C-e>',
      accept = '<C-y>',
    },
    highlight = { use_nvim_cmp_as_default = true },
    -- set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
    -- adjusts spacing to ensure icons are aligned
    nerd_font_variant = 'mono',
    -- experimental auto-brackets support
    accept = { auto_brackets = { enabled = true } },
    -- experimental signature help support
    trigger = { signature_help = { enabled = true } },
    sources = {
      providers = {
        { 'blink.cmp.sources.lsp', name = 'LSP', score_offset = 1 },
        {
          'blink.cmp.sources.snippets',
          name = 'Snippets',
          -- keyword_length = 1, -- not supported yet
        },
        {
          'blink.cmp.sources.path',
          name = 'Path',
          score_offset = 3,
          opts = { get_cwd = vim.uv.cwd },
        },
        {
          'blink.cmp.sources.buffer',
          name = 'Buffer',
          keyword_length = 3,
          score_offset = -1,
          fallback_for = { 'Path' }, -- PENDING https://github.com/Saghen/blink.cmp/issues/122
        },
      },
    },
    windows = {
      documentation = {
        min_width = 15,
        max_width = 50,
        max_height = 15,
        border = 'single',
        auto_show = true,
        auto_show_delay_ms = 200,
      },
    },
  },
}
