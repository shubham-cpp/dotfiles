return {
  'iamcco/markdown-preview.nvim',
  run = 'cd app && pnpm install',
  init = function()
    vim.g.mkdp_filetypes = { 'markdown' }
    vim.g.mkdp_refresh_slow = 1
  end,
  ft = { 'markdown' },
  cmd = { 'MarkdownPreview', 'MarkdownPreviewStop' },
}
