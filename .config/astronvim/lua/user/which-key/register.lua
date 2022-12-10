return {
  -- first key is the mode, n == normal mode
  n = {
    -- second key is the prefix, <leader> prefixes
    ['<leader>'] = {

      ['v'] = { 'v$', 'Select till end' },
      ['1'] = { '1gt', 'Goto tab 1' },
      ['2'] = { '2gt', 'Goto tab 2' },
      ['3'] = { '3gt', 'Goto tab 3' },
      ['4'] = { '4gt', 'Goto tab 4' },
      ['5'] = { '5gt', 'Goto tab 5' },
      ['6'] = { '6gt', 'Goto tab 6' },
      ['7'] = { '7gt', 'Goto tab 7' },
      ['8'] = { '8gt', 'Goto tab 8' },
      ['9'] = { '9gt', 'Goto tab 9' },
      c = {
        name = 'Change',
        d = { '<cmd>cd %:p:h<cr>:pwd<cr>', 'Directory To current File' },
      },

      b = {
        name = 'Buffer',
        a = { ':badd<space>', 'Add', silent = false },
        d = { '<cmd>bd<cr>', 'Delete' },
        n = { '<cmd>bn<cr>', 'Next' },
        p = { '<cmd>bp<cr>', 'Prev' },
      },

      e = {
        name = 'Edit',
        e = { ':edit <C-r>=expand("%:p:h")<cr>/', 'here', silent = false },
        v = { ':vnew <C-r>=expand("%:p:h")<cr>/', 'in vsplit', silent = false },
        t = { ':tabedit <C-r>=expand("%:p:h")<cr>/', 'in new tab', silent = false },
      },

      f = {
        n = {
          function()
            local builtin = require 'telescope.builtin'
            builtin.find_files({ cwd = vim.fn.stdpath 'config' })
          end,
          'Open Astro Configs',
        },
        N = {
          function()
            local builtin = require 'telescope.builtin'
            local config = vim.fn.getenv 'XDG_CONFIG_HOME'
            if not config then
              config = vim.fn.getenv 'HOME' .. '/.config'
            end
            local cwd = config .. '/astronvim/lua'
            builtin.find_files({ cwd = cwd })
          end,
          'Open Astro User Configs',
        },
        d = {
          function()
            local builtin = require 'telescope.builtin'
            local cwd = vim.fn.getenv 'HOME' .. '/Documents/dotfiles'
            builtin.find_files({ cwd = cwd, hidden = true })
          end,
          'Open Dotfiles',
        },
      },
      s = {
        b = { '<cmd>Telescope current_buffer_fuzzy_find<cr>', 'Current Buffer' },
        B = { '<cmd>Telescope git_branches<cr>', 'Git branches' },
        p = { '<cmd>Telescope live_grep<cr>', 'Project' },
      },
    },

    [']q'] = { '<cmd>cn<cr>', 'Quickfix Next ' },
    ['[q'] = { '<cmd>cn<cr>', 'Quickfix Prev ' },
    [']z'] = 'TreeSitter swap next',
    ['[z'] = 'TreeSitter swap prev',
    [']]'] = 'Next class start',
    [']['] = 'Next class end',
    ['[['] = 'Prev class start',
    ['[]'] = 'Prev class end',
  },
}
