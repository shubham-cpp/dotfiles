---@type LazySpec
return {
  {
    'AstroNvim/astrolsp',
    ---@type AstroLSPConfig
    opts = {
      handlers = {
        ruff = function(server, opts)
          local default_attach = opts.on_attach
          opts.on_attach = function(client, _)
            -- Disable hover in favor of Pyright
            client.server_capabilities.hoverProvider = false
          end
          require('lspconfig')[server].setup(opts)
        end,
      },
    },
  },
  { 'nvim-treesitter', opts = { ensure_installed = { 'python', 'pymanifest' } } },
  {
    'mason.nvim',
    opts = {
      ensure_installed = {
        'ruff',
        'basedpyright',
        'isort',
      },
    },
  },
  {
    'nvim-lint',
    opts = {
      linters_by_ft = {
        python = { 'ruff' },
      },
    },
  },
}
