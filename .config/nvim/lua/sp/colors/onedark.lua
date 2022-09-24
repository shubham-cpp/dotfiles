local onedark = require('onedark')
-- Options: deep | dark | darker | warm | warmer
onedark.setup({
  style = 'darker',
  transparent = true,
  highlights = {
    QuickScopePrimary = { fg = '$orange', fmt = 'underline,italic' },
    QuickScopeSecondary = { fg = '$cyan', fmt = 'underline,italic' },
  },
  code_style = {
    comments = 'italic',
    keywords = 'italic',
    functions = 'none',
    strings = 'none',
    variables = 'none',
  },
})
onedark.load()
