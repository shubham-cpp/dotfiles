---@type LazySpec
return {
  {
    'AstroNvim/astrolsp',
    ---@type AstroLSPConfig
    opts = {
      handlers = {
        tailwindcss = function(server, opts)
          opts.capabilities = require('user.config.util').get_lsp_capabilities(opts.capabilities)
          opts.init_options = {
            userLanguages = {
              eelixir = 'html-eex',
              eruby = 'erb',
              blade = 'html',
            },
          }
          opts.filetypes = vim.tbl_extend(
            'force',
            require('lspconfig').tailwindcss.document_config.default_config.filetypes,
            { 'blade' }
          )
          opts.root_dir = require('lspconfig.util').root_pattern(
            'tailwind.config.js',
            'tailwind.config.cjs',
            'tailwind.config.mjs',
            'tailwind.config.ts',
            'postcss.config.js',
            'postcss.config.cjs',
            'postcss.config.mjs',
            'postcss.config.ts',
            'assets/tailwind.config.js', -- for elixir phoenix
            'assets/tailwind.config.cjs', -- for elixir phoenix
            'assets/tailwind.config.mjs', -- for elixir phoenix
            'assets/tailwind.config.ts' -- for elixir phoenix
          )
          opts.settings = {
            tailwindCSS = {
              emmetCompletions = true,
              classAttributes = { 'class', 'className', 'classList', 'ngClass' },
              lint = {
                cssConflict = 'warning',
                invalidApply = 'error',
                invalidConfigPath = 'error',
                invalidScreen = 'error',
                invalidTailwindDirective = 'error',
                invalidVariant = 'error',
                recommendedVariantOrder = 'warning',
              },
              validate = true,
              experimental = {
                classRegex = {
                  'tw`([^`]*)',
                  'tw="([^"]*)',
                  'tw={"([^"}]*)',
                  'tw\\.\\w+`([^`]*)',
                  'tw\\(.*?\\)`([^`]*)',
                  { 'clsx\\(([^)]*)\\)', "(?:'|\"|`)([^']*)(?:'|\"|`)" },
                  { 'classnames\\(([^)]*)\\)', "'([^']*)'" },
                  { 'cva\\(([^)]*)\\)', '["\'`]([^"\'`]*).*?["\'`]' },
                  { 'cn\\(([^)]*)\\)', "(?:'|\"|`)([^']*)(?:'|\"|`)" },
                },
              },
            },
          }
          opts.single_file_support = false
          require('lspconfig')[server].setup(opts)
        end,
      },
    },
  },
  {
    'mason.nvim',
    opts = {
      ensure_installed = { 'tailwindcss-language-server' },
    },
  },
}
