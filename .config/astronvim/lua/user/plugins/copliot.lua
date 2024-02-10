return {
  'zbirenbaum/copilot.lua',
  cmd = 'Copilot',
  event = 'InsertEnter',
  enabled = true,
  config = function()
    require('copilot').setup({
      suggestion = { auto_trigger = true },
      filetypes = {
        yaml = true,
        markdown = true,
        gitcommit = true,
        ['*'] = true,
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
}
