---@type LazySpec
return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  enabled = true,
  config = require('plugins.config.autopairs').config,
}
