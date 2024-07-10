---Description: NvimTree configuration
---Use single hotkeys to toggle/focus nvim-tree
---If nvim-tree is focused - close it, otherwise focus it
---@param isFind boolean
local nvimTreeFocusOrToggle = function(isFind)
  local nvimTree = require 'nvim-tree.api'
  local localIsFind = isFind or false
  local currentBuf = vim.api.nvim_get_current_buf()
  local currentBufFt = vim.api.nvim_get_option_value('filetype', { buf = currentBuf })
  if currentBufFt == 'NvimTree' then
    nvimTree.tree.toggle()
  else
    nvimTree.tree.focus({ find_file = localIsFind })
  end
end

---@type LazySpec
return {
  'nvim-tree/nvim-tree.lua',
  enabled = true,
  dependencies = { 'nvim-tree/nvim-web-devicons', 'antosha417/nvim-lsp-file-operations', 'nvim-lua/plenary.nvim' },
  cmd = 'NvimTreeToggle',
  keys = {
    {
      '<leader>e',
      function()
        nvimTreeFocusOrToggle(true)
      end,
      desc = 'Toggle NvimTree(Find)',
    },
    { '<leader>E', nvimTreeFocusOrToggle, desc = 'Toggle NvimTree' },
  },
  init = function()
    -- disable netrw at the very start of your init.lua
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1
  end,
  config = function()
    require 'plugins.config.nvim-tree'
  end,
}
