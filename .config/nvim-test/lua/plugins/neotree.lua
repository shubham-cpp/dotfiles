---@type LazySpec
return {
  'nvim-neo-tree/neo-tree.nvim',
  version = 'v3.x',
  cmd = 'Neotree',
  enabled = true,
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
    'antosha417/nvim-lsp-file-operations',
    {
      's1n7ax/nvim-window-picker',
      version = '2.*',
      config = function()
        require('window-picker').setup({
          filter_rules = {
            include_current_win = false,
            autoselect_one = true,
            -- filter using buffer options
            bo = {
              -- if the file type is one of following, the window will be ignored
              filetype = { 'neo-tree', 'neo-tree-popup', 'notify' },
              -- if the buffer type is one of following, the window will be ignored
              buftype = { 'terminal', 'quickfix' },
            },
          },
          --- @type 'statusline-winbar' | 'floating-big-letter'
          hint = 'floating-big-letter',
        })
      end,
    },
  },
  keys = {
    {
      '<leader>e',
      function()
        local bufnr = vim.api.nvim_get_current_buf()
        local bufname = vim.fn.bufname(bufnr)
        if bufname ~= '' and string.match(bufname, 'neo.tree %w+') ~= nil then
          vim.cmd 'Neotree toggle'
        else
          vim.cmd 'Neotree reveal focus'
        end
      end,
      desc = 'Open File [E]xplorer(Reveal)',
    },
    { '<leader>E', '<cmd>Neotree focus<cr>', desc = 'Open File [E]xplorer' },
  },
  init = function()
    vim.g.neo_tree_remove_legacy_commands = 1
  end,
  config = require('plugins.config.neotree').config,
}
