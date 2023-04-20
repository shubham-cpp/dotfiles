return {
  'NvChad/nvim-colorizer.lua',
  event = 'BufReadPost',
  opts = {
    filetypes = { '*' },
    user_default_options = {
      -- RRGGBBAA = true,
      rgb_fn = true,
      hsl_fn = true,
      -- css = true,
      -- css_fn = true,
      tailwind = true,
      virtualtext = '■',
      -- Available modes for `mode`: foreground, background,  virtualtext
      mode = 'background',
    },
    buftypes = {
      -- exclude prompt and popup buftypes from highlight
      '!prompt',
      '!popup',
    },
  },
}
