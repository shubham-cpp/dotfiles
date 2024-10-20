---@type LazySpec
return {
  'altermo/ultimate-autopair.nvim',
  event = { 'InsertEnter', 'CmdlineEnter' },
  branch = 'v0.6', --recommended as each new version will have breaking changes
  enabled = true,
  config = function()
    local ok_cmp, _ = pcall(require, 'cmp')
    local inits = {
      require('ultimate-autopair').extend_default({}),
    }
    if ok_cmp then
      table.insert(inits, { profile = require('ultimate-autopair.experimental.cmpair').init })
    end
    require('ultimate-autopair').init(inits)
  end,
}
