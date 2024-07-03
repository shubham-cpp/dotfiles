return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  config = require('plugins.config.autopairs').config,
}
