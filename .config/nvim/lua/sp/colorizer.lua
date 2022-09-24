local colorizer = require('colorizer')
colorizer.setup({
  'html',
  'css',
  'scss',
  'lua',
  'javascript',
  'typescript',
  'javascriptreact',
  'typescriptreact',
  'xdefaults',
}, {
  RGB = true,
  RRGGBB = true,
  names = true,
  RRGGBBAA = true,
  rgb_fn = true,
  hsl_fn = true,
  css = true,
  css_fn = true,
})
