---@type LazySpec
return {
  'altermo/ultimate-autopair.nvim',
  event = { 'InsertEnter', 'CmdlineEnter' },
  branch = 'v0.6', --recommended as each new version will have breaking changes
  enabled = true,
  config = function()
    require('ultimate-autopair').init({
      require('ultimate-autopair').extend_default({}),
      { profile = require('ultimate-autopair.experimental.cmpair').init },
    })
  end,
}
