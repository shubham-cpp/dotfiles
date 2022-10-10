require('tokyonight').setup({
  style = 'night', -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
  transparent = true,
  styles = {
    keywords = { italic = false },
  },
  hide_inactive_statusline = true,
})
vim.cmd 'colorscheme tokyonight'
