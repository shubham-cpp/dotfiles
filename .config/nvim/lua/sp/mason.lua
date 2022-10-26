require('mason').setup({
  ui = {
    icons = {
      package_installed = '✓',
      package_pending = '➜',
      package_uninstalled = '✗',
    },
  },
})

require('mason-lspconfig').setup({
  ensure_installed = {
    'sumneko_lua',
    'rust_analyzer',
    'bashls',
    'clangd',
    'cssls',
    'html',
    'jsonls',
    'dockerls',
    'tsserver',
    'prismals',
    'pyright',
    'svelte',
    'tailwindcss',
    'vimls',
    'volar',
    'yamlls',
  },
})
