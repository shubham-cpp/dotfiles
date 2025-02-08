---@type LazySpec
return {
  {
    'AstroNvim/astrolsp',
    opts = {
      handlers = {
        jsonls = function(server, opts)
          opts.capabilities = require('user.config.util').get_lsp_capabilities(opts.capabilities)
          opts.on_new_config = function(new_config)
            new_config.settings.json.schemas = new_config.settings.json.schemas or {}
            vim.list_extend(new_config.settings.json.schemas, require('schemastore').json.schemas())
          end
          opts.settings = { json = { validate = { enable = true }, format = { enable = false } } }
          require('lspconfig')[server].setup(opts)
        end,
      },
    },
  },
  { 'b0o/schemastore.nvim', lazy = true, version = false },
  { 'nvim-treesitter', opts = { ensure_installed = { 'json', 'jsonc', 'json5' } } },
  { 'mason.nvim', opts = { ensure_installed = { 'json-lsp', 'yamllint' } } },
  { 'nvim-lint', opts = { linters_by_ft = { yaml = { 'yamllint' } } } },
  {
    'vuki656/package-info.nvim',
    dependencies = { 'MunifTanjim/nui.nvim' },
    opts = {},
    event = 'BufRead package.json',
  },
}
