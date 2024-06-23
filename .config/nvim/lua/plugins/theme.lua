---@type LazySpec
return {
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    event = 'VimEnter',
    enabled = false,
    config = function()
      require('catppuccin').setup({
        ---@type "latte"| "frappe"| "macchiato"| "mocha"|"auto"
        flavour = 'mocha',
        transparent_background = false,
        dim_inactive = {
          enabled = false, -- dims the background color of inactive window
          shade = 'dark',
          percentage = 0.15, -- percentage of the shade to apply to the inactive window
        },
        custom_highlights = function(colors)
          return {
            QuickScopePrimary = { fg = '#dfbb78', bg = '#505050', style = { 'underline', 'bold' } },
            QuickScopeSecondary = { fg = '#61afef', bg = '#505050', style = { 'underline', 'bold' } },
          }
        end,
      })
      vim.cmd 'colorscheme catppuccin'
    end,
  },
  {
    'rebelot/kanagawa.nvim',
    event = 'VimEnter',
    enabled = true,
    config = function()
      require('kanagawa').setup({
        compile = true,
        transparent = true,
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
