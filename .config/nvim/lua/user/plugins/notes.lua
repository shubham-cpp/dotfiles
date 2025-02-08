---@type LazySpec
return {
  'yujinyuz/gitpad.nvim',
  opts = {
    on_attach = function(bufnr)
      vim.keymap.set('n', 'q', '<cmd>wq<cr>', { silent = true, buffer = bufnr })
    end,
    floating_win_opts = {
      ---@type 'single'| 'double'| 'shadow'| 'rounded'
      border = 'rounded',
    },
  },
  keys = function()
    local g = require 'gitpad'
    return {
      {
        '<leader>pp',
        g.toggle_gitpad, -- or g.toggle_gitpad({ title = 'Project notes' })
        desc = 'gitpad project',
      },
      {
        '<leader>pb',
        g.toggle_gitpad_branch, -- or g.toggle_gitpad_branch({ title = 'Branch notes' })
        desc = 'gitpad branch',
      },
      -- Daily notes
      {
        '<leader>pd',
        function()
          local date_filename = 'daily-' .. os.date '%Y-%m-%d.md'
          g.toggle_gitpad({ filename = date_filename }) -- or g.toggle_gitpad({ filename = date_filename, title = 'Daily notes' })
        end,
        desc = 'gitpad daily notes',
      },
      -- Per file notes
      {
        '<leader>pf',
        function()
          local filename = vim.fn.expand '%:p' -- or just use vim.fn.bufname()
          if filename == '' then
            vim.notify 'empty bufname'
            return
          end
          filename = vim.fn.pathshorten(filename, 2) .. '.md'
          g.toggle_gitpad({ filename = filename }) -- or g.toggle_gitpad({ filename = filename, title = 'Current file notes' })
        end,
        desc = 'gitpad per file notes',
      },
    }
  end,
}
