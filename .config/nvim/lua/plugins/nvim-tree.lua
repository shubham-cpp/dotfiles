local au_group = vim.api.nvim_create_augroup('sp_nvim_tree', { clear = true })

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

local function my_on_attach(bufnr)
  local api = require 'nvim-tree.api'

  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  -- default mappings
  api.config.mappings.default_on_attach(bufnr)

  -- custom mappings

  vim.keymap.del('n', 'U', { buffer = bufnr })
  vim.keymap.del('n', 'L', { buffer = bufnr })
  vim.keymap.del('n', 'M', { buffer = bufnr })
  vim.keymap.set('n', 'H', api.node.navigate.parent, opts 'Goto parent')
  vim.keymap.set('n', 'l', api.node.open.edit, opts 'Open')
  vim.keymap.set('n', 'L', api.node.open.no_windown_picker, opts 'Open(no window select)')
  vim.keymap.set('n', '?', api.tree.toggle_help, opts 'Help')
  vim.keymap.set('n', 'D', api.fs.remove, opts 'Delete')
  vim.keymap.set('n', 'd', api.fs.trash, opts 'Trash')

  vim.keymap.set('n', 'tl', api.node.open.toggle_group_empty, opts 'Toggle Group Empty')
  vim.keymap.set('n', 'tm', api.tree.toggle_no_bookmark_filter, opts 'Toggle Filter: No Bookmark')
  vim.keymap.set('n', 'td', api.tree.toggle_hidden_filter, opts 'Toggle Dotfiles')
  vim.keymap.set('n', 'tg', api.tree.toggle_gitignore_filter, opts 'Toggle Gitignore')
  vim.keymap.set('n', 'tc', api.tree.toggle_custom_filter, opts 'Toggle Custom filters')
end

---@type LazySpec
return {
  'nvim-tree/nvim-tree.lua',
  enabled = false,
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

    vim.api.nvim_create_autocmd('BufEnter', {
      group = au_group,
      desc = "Close nvim-tree if it's the only window",
      pattern = 'NvimTree_*',
      callback = function()
        local layout = vim.api.nvim_call_function('winlayout', {})
        if
          layout[1] == 'leaf'
          and vim.api.nvim_get_option_value('filetype', { buf = vim.api.nvim_win_get_buf(layout[2]) }) == 'NvimTree'
          and layout[3] == nil
        then
          vim.cmd 'confirm quit'
        end
      end,
    })
  end,
  config = function()
    require('nvim-tree').setup({
      view = { width = 40 },
      ui = {
        confirm = {
          remove = true,
          trash = false,
        },
      },
      on_attach = my_on_attach,
    })
    require('lsp-file-operations').setup()
  end,
}
