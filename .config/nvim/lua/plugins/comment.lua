---@type LazySpec
return {
  'folke/ts-comments.nvim',
  opts = {
    lang = {
      jsdoc = '/** %s */',
      phpdoc = { '// %s' },
    },
  },
  event = 'VeryLazy',
  enabled = vim.fn.has 'nvim-0.10.0' == 1,
}
