local au_group = vim.api.nvim_create_augroup('sp_indent', { clear = true })

---@type LazySpec
return {
  'echasnovski/mini.indentscope',
  version = false,
  enabled = true,
  event = 'BufReadPost',
  config = function()
    require('mini.indentscope').setup({
      -- Which character to use for drawing scope indicator
      symbol = '╎',
      options = { try_as_border = true },
    })
  end,
  init = function()
    vim.api.nvim_create_autocmd('FileType', {
      group = au_group,
      desc = 'Disable mini.indentscope for specific filetypes',
      pattern = {
        'help',
        'alpha',
        'dashboard',
        'neo-tree',
        'Trouble',
        'trouble',
        'lazy',
        'mason',
        'notify',
        'toggleterm',
        'lazyterm',
      },
      callback = function()
        vim.b.miniindentscope_disable = true
      end,
    })
  end,
}
