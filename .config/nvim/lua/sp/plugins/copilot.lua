return {
  {
    'github/copilot.vim',
    event = 'InsertEnter',
    enabled = false,
    init = function()
      vim.g.copilot_no_tab_map = true
      -- highlight CopilotSuggestion guifg=#555555 ctermfg=8
    end,
    keys = {
      { '<A-g>', 'copilot#Accept("\\<CR>")', mode = 'i', expr = true, replace_keycodes = false },
      { '<A-l>', '<Plug>(copilot-accept-line)', mode = 'i' },
    },
  },
  {
    'zbirenbaum/copilot.lua',
    cmd = 'Copilot',
    event = 'InsertEnter',
    enabled = false,
    config = function()
      require('copilot').setup({
        suggestion = {
          auto_trigger = true,
        },
        filetypes = {
          yaml = true,
          markdown = true,
          help = false,
          gitcommit = true,
          gitrebase = true,
          ['.'] = true,
          sh = function()
            if string.match(vim.fs.basename(vim.api.nvim_buf_get_name(0)), '^%.env.*') then
              -- disable for .env files
              return false
            end
            return true
          end,
        },
      })
    end,
  },
}
