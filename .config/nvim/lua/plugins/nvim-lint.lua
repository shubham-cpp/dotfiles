local au_group = vim.api.nvim_create_augroup('sp_nvim_lint', {})
local languages = {
  astro = { 'eslint_d' },
  bash = { 'shellcheck' },
  -- c = { 'clangtidy' },
  -- cpp = { 'clangtidy' },
  fish = { 'fish' },
  go = { 'golangcilint' },
  markdown = { 'vale' },
  python = { 'ruff' },
  sh = { 'shellcheck' },
  svelte = { 'eslint_d' },
  javascript = { 'eslint_d' },
  javascriptreact = { 'eslint_d' },
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
  ft = vim.tbl_keys(languages),
  config = function()
    require('lint').linters_by_ft = languages
    local function run()
      require('lint').try_lint()
    end
    vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufRead', 'InsertLeave', 'FocusGained' }, {
      group = au_group,
      desc = 'Lint on save',
      pattern = '*',
      callback = run,
    })
  end,
}
