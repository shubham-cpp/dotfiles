return {
  {
    'projekt0n/github-nvim-theme',
    event = 'VimEnter',
    enabled = true,
    config = function()
      require('github-theme').setup({
        options = {
          transparent = false,
          dim_inactive = true,
          inverse = { -- Inverse highlight for different types
            match_paren = false,
            visual = false,
            search = false,
          },
        }
      })
      -- github_dark_tritanopia | github_dark_colorblind | github_dark_high_contrast | github_dark_dimmed | github_dark_default | github_dark
      vim.cmd.colorscheme 'github_dark_default'
    end,
  },
  {
    'rebelot/kanagawa.nvim',
    event = 'VimEnter',
    enabled = false,
    config = function()
      require('kanagawa').setup({
        compile = true,
        -- transparent = true, -- do not set background color
        dimInactive = true,
        theme = 'wave', -- Load "wave" | "dragon" | "lotus"
      })
      vim.cmd 'colorscheme kanagawa-wave'
      -- vim.cmd("colorscheme kanagawa-dragon")
      -- vim.cmd("colorscheme kanagawa-lotus")
    end,
  },
  {
    'rmehri01/onenord.nvim',
    enabled = false,
    event = 'VimEnter',
    opts = {
      fade_nc = true,
      disable = {
        background = true,
      },
      styles = {
        comments = 'italic',
        strings = 'NONE',
        keywords = 'NONE',
        functions = 'bold',
        variables = 'NONE',
        diagnostics = 'underline',
      },
      custom_highlights = {
        QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = 'underline,bold' },
        QuickScopeSecondary = {
          fg = '#61afef',
          bg = '#505050',
          style = 'underline,bold',
        },
        PmenuSel = { bg = '#61afef', fg = '#24253b' },
      },
    },
    config = function(_, opts)
      require('onenord').setup(opts)
      vim.cmd 'colorscheme onenord'
    end,
  },
}
