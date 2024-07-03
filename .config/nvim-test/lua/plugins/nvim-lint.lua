return {
  'mfussenegger/nvim-lint',
  enabled = true,
  ft = require('plugins.config.nvim-lint').languages,
  config = require('plugins.config.nvim-lint').config,
}
