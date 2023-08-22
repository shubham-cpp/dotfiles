return {
  {
    'lewis6991/gitsigns.nvim',
    dependencies = {
      {
        'akinsho/git-conflict.nvim',
        version = '*',
        config = true,
        -- config = function()
        --   vim.api.nvim_create_autocommand('User', {
        --     group = vim.api.nvim_create_augroup('sp_git', { clear = true }),
        --     pattern = 'GitConflictDetected',
        --     callback = function()
        --       vim.notify('Conflict detected in ' .. vim.fn.expand '<afile>')
        --       vim.keymap.set('n', 'cww', function()
        --         engage.conflict_buster()
        --         create_buffer_local_mappings()
        --       end)
        --     end,
        --   })
        -- end
      },
    },
    event = 'BufReadPost',
    opts = {
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end
        -- Navigation
        map('n', ']c', function()
          if vim.wo.diff then
            return ']c'
          end
          vim.schedule(function()
            gs.next_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, desc = "Next hunk([C]hange)" })

        map('n', '[c', function()
          if vim.wo.diff then
            return '[c'
          end
          vim.schedule(function()
            gs.prev_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, desc = "Previous hunk([C]hange)" })
        map('n', '<leader>gs', gs.stage_hunk, { desc = "Stage hunk" })
        map('n', '<leader>gr', gs.reset_hunk, { desc = "Reset hunk" })
        map('v', '<leader>gs', function()
          gs.stage_hunk({ vim.fn.line '.', vim.fn.line 'v' })
        end, { desc = "Stage hunk" })
        map('v', '<leader>gr', function()
          gs.reset_hunk({ vim.fn.line '.', vim.fn.line 'v' })
        end, { desc = "Reset hunk" })
        map('n', '<leader>gS', gs.stage_buffer, { desc = "Stage buffer" })
        map('n', '<leader>gu', gs.undo_stage_hunk, { desc = "Undo stage hunk" })
        map('n', '<leader>gR', gs.reset_buffer, { desc = "Reset buffer" })
        map('n', '<leader>gp', gs.preview_hunk, { desc = "Preview hunk" })
        map('n', '<leader>gb', function()
          gs.blame_line({ full = true })
        end, { desc = "Blame line" })
        map('n', '<leader>gd', gs.diffthis, { desc = "Diff this" })
        map('n', '<leader>gD', function()
          gs.diffthis '~'
        end)
        -- Text object
        map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
      end,
    },
  },
  {
    "NeogitOrg/neogit",
    cmd = { 'Neogit' },
    keys = { { '<leader>og', '<cmd>Neogit<cr>', desc = 'Neogit' } },
    dependencies = {
      "nvim-lua/plenary.nvim",  -- required
      "sindrets/diffview.nvim", -- optional
    },
    config = true
  },
}
