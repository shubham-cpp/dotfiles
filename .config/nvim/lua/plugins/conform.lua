return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  keys = {
    {
      -- Customize or remove this keymap to your liking
      '<leader>=',
      function()
        require('conform').format({ async = true, lsp_fallback = true })
      end,
      mode = { 'n', 'v' },
      desc = 'Format buffer(c)',
    },
  },
  init = function()
    -- If you want the formatexpr, here is the place to set it
    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
  end,
  config = function()
    require 'plugins.config.conform'
  end,
}
