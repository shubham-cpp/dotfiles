return {
  'monaqa/dial.nvim',
  version = '0.4.*',
  keys = {
    { '<C-a>', '<Plug>(dial-increment)', mode = { 'n', 'x' } },
    { '<C-x>', '<Plug>(dial-decrement)', mode = { 'n', 'x' } },
    { 'g<C-a>', 'g<Plug>(dial-increment)', mode = 'v' },
    { 'g<C-x>', 'g<Plug>(dial-decrement)', mode = 'v' },
  },
  config = function()
    local augend = require 'dial.augend'
    require('dial.config').augends:register_group({
      default = {
        augend.integer.alias.decimal,
        augend.constant.alias.bool,
        augend.integer.alias.hex,
        augend.date.alias['%Y/%m/%d'],
        augend.constant.new({
          elements = { '&&', '||' },
          word = false,
          cyclic = true,
        }),
      },
      typescript = {
        augend.integer.alias.decimal,
        augend.integer.alias.hex,
        augend.constant.new({ elements = { 'let', 'const' } }),
        augend.constant.new({
          elements = { '&&', '||' },
          word = false,
          cyclic = true,
        }),
      },
      py_lua = {
        augend.integer.alias.decimal,
        augend.constant.alias.bool,
        augend.integer.alias.hex,
        augend.constant.new({
          elements = { 'and', 'or' },
          word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
          cyclic = true, -- "or" is incremented into "and".
        }),
        augend.date.alias['%Y/%m/%d'],
      },
      visual = {
        augend.integer.alias.decimal,
        augend.integer.alias.hex,
        augend.date.alias['%Y/%m/%d'],
        augend.constant.alias.alpha,
        augend.constant.alias.Alpha,
      },
    })
    local au_ts = vim.api.nvim_create_augroup('dial_au_ts', { clear = true })
    vim.api.nvim_create_autocmd('FileType', {
      group = au_ts,
      pattern = { 'lua', 'python', 'fennel' },
      callback = function()
        vim.keymap.set(
          { 'n', 'x' },
          '<C-a>',
          require('dial.map').inc_normal 'py_lua',
          { noremap = true, buffer = true }
        )
      end,
    })
    vim.api.nvim_create_autocmd('FileType', {
      group = au_ts,
      pattern = { 'typescript', 'javascript', 'typescriptreact', 'javascriptreact' },
      callback = function(args)
        local bufnr = args.buf
        vim.keymap.set(
          { 'n', 'x' },
          '<C-a>',
          require('dial.map').inc_normal 'typescript',
          { noremap = true, buffer = bufnr }
        )
        vim.keymap.set(
          { 'n', 'x' },
          '<C-x>',
          require('dial.map').dec_normal 'typescript',
          { noremap = true, buffer = bufnr }
        )
      end,
    })
  end,
}
