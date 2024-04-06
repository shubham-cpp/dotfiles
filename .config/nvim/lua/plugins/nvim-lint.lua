local au_group = vim.api.nvim_create_augroup('sp_nvim_lint', { clear = true })
local languages = {
  astro = { 'eslint_d' },
  bash = { 'shellcheck' },
  c = { 'clangtidy' },
  cpp = { 'clangtidy' },
  fish = { 'fish' },
  go = { 'golangcilint' },
  javascript = { 'eslint_d' },
  javascriptreact = { 'eslint_d' },
  markdown = { 'vale' },
  python = { 'ruff' },
  sh = { 'shellcheck' },
  svelte = { 'eslint_d' },
  typescript = { 'eslint_d' },
  typescriptreact = { 'eslint_d' },
  vue = { 'eslint_d' },
  zsh = { 'shellcheck', 'zsh' },
  html = { 'htmlhint' },
  yaml = { 'yamllint' },
}
return {
  'mfussenegger/nvim-lint',
  enabled = true,
  event = 'BufRead',
  config = function()
    require('lint').linters_by_ft = languages
    vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufReadPost', 'InsertLeave' }, {
      group = au_group,
      callback = function()
        require('lint').try_lint()
      end,
    })
  end,
}
