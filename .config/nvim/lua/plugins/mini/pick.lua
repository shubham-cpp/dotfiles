---@type LazySpec
return {
  {
    'echasnovski/mini.pick',
    enabled = true,
    dependencies = {
      {
        'echasnovski/mini.extra',
        version = '*',
        config = function()
          require('mini.extra').setup()
        end,
      },
    },
    version = '*',
    keys = function()
      local pick = require 'mini.pick'

      return {
        { '<leader>p', '', desc = '+pick' },
        {
          '<leader>pb',
          function()
            pick.builtin.buffers({ include_current = false })
          end,
          desc = '[B]uffer',
        },
        { '<leader>pf', pick.builtin.files, desc = '[F]ile' },
        {
          '<leader>pn',
          function()
            local opts = { source = { cwd = vim.fn.stdpath 'config' } }
            local local_opts = { cwd = nil, tool = 'fd' }
            pick.builtin.files(local_opts, opts)
          end,
          desc = '[N]eovim config',
        },
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
    end,
  },
}
