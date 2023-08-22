return {
  'ggandor/leap.nvim',
  enabled = false,
  keys = { 's', 'S' },
  config = function()
    require('leap').add_default_mappings()
  end,
}
