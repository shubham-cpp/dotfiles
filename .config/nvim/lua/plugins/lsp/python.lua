if true then
  return {}
end
---@type LazySpec
return {
  { 'williamboman/mason.nvim', opts = { ensure_installed = { 'pyright' } } },
  { 'nvim-treesitter/nvim-treesitter', opts = { ensure_installed = { 'pymanifest', 'requirements', 'python' } } },
  {
    'AstroNvim/astrolsp',
    opts = {
      config = {
        pyright = {
          settings = {
            python = {
              analysis = {
                autoImportCompletions = true,
                -- typeCheckingMode = 'basic',
              },
              exclude = { '**/node_modules', '**/__pycache__' },
            },
          },
        },
      },
    },
  },
}
