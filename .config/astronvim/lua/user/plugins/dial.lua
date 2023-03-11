return {
  'monaqa/dial.nvim',
  version = '*',
  keys = {
    { '<C-a>',  '<Plug>(dial-increment)',  mode = { 'n', 'v' } },
    { '<C-x>',  '<Plug>(dial-decrement)',  mode = { 'n', 'v' } },
    { 'g<C-a>', 'g<Plug>(dial-increment)', mode = 'v',         noremap = false },
    { 'g<C-x>', 'g<Plug>(dial-decrement)', mode = 'v',         noremap = false },
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
        augend.constant.alias.bool,
        augend.integer.alias.hex,
        augend.constant.new({ elements = { 'let', 'const' } }),
        augend.date.alias['%Y/%m/%d'],
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
          word = true,   -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
          cyclic = true, -- "or" is incremented into "and".
        }),
        augend.date.alias['%Y/%m/%d'],
      },
      visual = {
        augend.integer.alias.decimal,
        augend.integer.alias.hex,
        augend.integer.alias.hex,
        augend.date.alias['%Y/%m/%d'],
        augend.constant.alias.alpha,
        augend.constant.alias.Alpha,
      },
    })
    local auGroupDial = vim.api.nvim_create_augroup('auGroupDialNvim', { clear = true })
    vim.api.nvim_create_autocmd('FileType', {
      pattern = { 'typescript', 'javascript', 'typescriptreact', 'javascriptreact', 'html', 'vue', 'svelte' },
      desc = 'Use typescript group of dial.nvim',
      callback = function()
        vim.keymap.set(
          { 'n', 'x' },
          '<C-a>',
          '"=typescript<CR><Plug>(dial-increment)',
          { buffer = true, noremap = false, silent = true }
        )
        vim.keymap.set(
          { 'n', 'x' },
          '<C-x>',
          '"=typescript<CR><Plug>(dial-decrement)',
          { buffer = true, noremap = false, silent = true }
        )
        -- vim.keymap.set(
        --   { 'x' },
        --   'g<C-x>',
        --   '"=typescript<CR><Plug>(dial-decrement)',
        --   { buffer = true, noremap = false, silent = true }
        -- )
      end,
      group = auGroupDial,
    })

    vim.api.nvim_create_autocmd('FileType', {
      pattern = { 'python', 'lua' },
      desc = 'Use typescript group of dial.nvim',
      callback = function()
        vim.keymap.set(
          { 'n', 'x' },
          '<C-a>',
          '"=py_lua<CR><Plug>(dial-increment)',
          { buffer = true, noremap = false, silent = true }
        )
        vim.keymap.set(
          { 'n', 'x' },
          '<C-x>',
          '"=py_lua<CR><Plug>(dial-decrement)',
          { buffer = true, noremap = false, silent = true }
        )
      end,
      group = auGroupDial,
    })
  end,
}
