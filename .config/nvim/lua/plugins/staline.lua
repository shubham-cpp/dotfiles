---@type LazySpec
return {
  'tamton-aquib/staline.nvim',
  enabled = false,
  event = 'BufWinEnter',
  config = function()
    require('staline').setup({
      sections = {
        left = {
          { 'Evil', ' ' },
          ' ', -- The mode and evil sign
          'file_size',
          ' ', -- Filesize
          { 'StalineFile', 'file_name' },
          ' ', -- Filename in different highlight
        },
        mid = { { 'StalineLsp', 'lsp_name' } }, -- "lsp_name" is still a little buggy
        right = {
          { 'StalineEnc', vim.bo.fileencoding:upper() },
          '  ', -- Example for custom section
          { 'StalineEnc', 'cool_symbol' },
          ' ', -- the cool_symbol for your OS
          { 'StalineGit', 'branch' },
        },
      },
      defaults = {
        -- bg = '#202328',
        branch_symbol = ' ',
      },
      mode_colors = {
        n = '#38b1f0',
        i = '#9ece6a', -- etc mode
      },
    })
    require('stabline').setup({
      style = 'bar',
      bg = '#52494e',
      fg = '#E4E4E4',
      inactive_bg = '#282828',
      inactive_fg = '#E4E4E4',
    })

    vim.api.nvim_set_hl(0, 'Evil', { fg = '#ff4f58', bg = 'none' })
    vim.api.nvim_set_hl(0, 'StalineGit', { fg = '#73d936', bg = 'none', bold = true })
    vim.api.nvim_set_hl(0, 'StalineFile', { fg = '#9e95c7', bg = 'none' })
    vim.api.nvim_set_hl(0, 'StalineLsp', { fg = '#52494e', bg = 'none' })
  end,
}
