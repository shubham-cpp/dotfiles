---@type LazySpec
return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  enabled = false,
  config = require('plugins.config.autopairs').config,
}
