return {
  'ggandor/leap.nvim',
  enabled = true,
  keys = { 's', 'S' },
  config = function()
    require('leap').add_default_mappings()
  end,
}
