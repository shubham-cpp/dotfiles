local config = {
  {
    'rmehri01/onenord.nvim',
    enabled = true,
    opts = {
      fade_nc = true,
      styles = {
        comments = 'italic',
        strings = 'NONE',
        keywords = 'NONE',
        functions = 'bold',
        variables = 'NONE',
        diagnostics = 'underline',
      },
      custom_highlights = {
        Normal = { bg = '#1e222a' },
        NormalNC = { bg = '#1d1915' },
        QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = 'underline,bold' },
        QuickScopeSecondary = {
          fg = '#61afef',
          bg = '#505050',
          style = 'underline,bold',
        },
      },
    },
  },
  {
    'navarasu/onedark.nvim',
    enabled = false,
    config = function()
      require('onedark').setup({
        style = 'deep', -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
        transparent = true,
        -- cmp_itemkind_reverse = false
      })
      require('onedark').load()
    end,
  },

}

return config