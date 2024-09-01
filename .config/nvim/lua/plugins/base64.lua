if true then
  return {}
end
---@type LazySpec
return {
  { 'nvim-lua/plenary.nvim', lazy = true },

  {
    'NvChad/base46',
    lazy = true,
    build = function()
      require('base46').load_all_highlights()
    end,
  },

  -- if u want nvchad's ui plugin :)
  {
    'NvChad/ui',
    config = function()
      require 'nvchad'
      -- dofile(vim.g.base46_cache .. 'defaults')
      -- dofile(vim.g.base46_cache .. 'statusline')
    end,
  },

  -- dependency for ui
  {
    'nvim-tree/nvim-web-devicons',
    lazy = true,
    opts = function()
      return { override = require 'nvchad.icons.devicons' }
    end,
    config = function(_, opts)
      dofile(vim.g.base46_cache .. 'devicons')
      require('nvim-web-devicons').setup(opts)
    end,
  },
  {
    'SmiteshP/nvim-navic',
    lazy = true,
    -- event = "LspAttach",
    config = function()
      dofile(vim.g.base46_cache .. 'navic')
      vim.api.nvim_set_hl(0, 'WinBar', { link = 'TreesitterContext' })
      vim.api.nvim_set_hl(0, 'WinBarNC', { link = 'Normal' })
      require('nvim-navic').setup({
        highlight = true,
        lsp = { auto_attach = true },
      })

      vim.o.winbar = "%{%v:lua.require'nvim-navic'.get_location()%}"
    end,
  },
}
