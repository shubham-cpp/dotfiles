---@type LazySpec
return {
  'mattn/emmet-vim',
  cmd = 'EmmetInstall',
  keys = { { '<c-y>', desc = 'Emmet plugin', mode = { 'i', 'n' } } },
  init = function()
    vim.g.user_emmet_install_global = 0
  end,
  config = function()
    vim.api.nvim_create_autocmd('FileType', {
      pattern = { 'html', 'css', 'scss', 'javascriptreact', 'typescriptreact', 'astro', 'vue', 'svelte' },
      desc = 'Enabled emmet plugin for specific filetypes',
      group = vim.api.nvim_create_augroup('sp_emmet', { clear = true }),
      command = 'EmmetInstall',
    })
  end,
}
