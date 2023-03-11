return {
  'Exafunction/codeium.vim',
  event = 'InsertEnter',
  config = function()
    vim.g.codeium_disable_bindings = 1
    -- Change '<C-g>' here to any keycode you like.
    vim.keymap.set('i', '<A-g>', function()
      return vim.fn['codeium#Accept']()
    end, { expr = true })
    vim.keymap.set('i', '<C-;>', function()
      return vim.fn['codeium#CycleCompletions'](1)
    end, { expr = true })
    vim.keymap.set('i', '<C-,>', function()
      return vim.fn['codeium#CycleCompletions'](-1)
    end, { expr = true })
    vim.keymap.set('i', '<C-c>', function()
      return vim.fn['codeium#Clear']()
    end, { expr = true })
  end,
}
