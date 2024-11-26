---@type LazySpec
return {
  'kylechui/nvim-surround',
  enabled = true,
  config = true,
  -- config = function()
  --   require("nvim-surround").setup()
  -- end,
  keys = { 'ys', 'ds', 'cs', { 'S', mode = 'x', silent = false } },
}
