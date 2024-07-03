---@type LazySpec
return {
  {
    'echasnovski/mini.pick',
    dependencies = {
      { 'echasnovski/mini.extra', version = '*' },
    },
    version = '*',
    keys = function()
      local pick = require 'mini.pick'

      return {
        { '<leader>pf', pick.builtin.files, desc = '[F]ile' },
      }
    end,
    config = function()
      require('mini.pick').setup({
        mappings = {
          choose_in_split = '<C-x>',
          mark = '<C-m>',
          mark_all = '<C-a>',
          move_down = '<C-j>',
          move_start = '<C-g>',
          move_up = '<C-k>',
        },
        -- General options
        options = {
          -- Whether to show content from bottom to top
          content_from_bottom = false,
          -- Whether to cache matches (more speed and memory on repeated prompts)
          use_cache = true,
        },
      })
      require('mini.extra').setup()
    end,
  },
}
