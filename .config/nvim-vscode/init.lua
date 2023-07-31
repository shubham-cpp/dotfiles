local o = vim.opt
local g = vim.g
local cmd = vim.cmd
local old_path = vim.fn.stdpath
g.mapleader = ' '

local disabled_built_ins = {
  'netrw',
  'netrwPlugin',
  'netrwSettings',
  'netrwFileHandlers',
  'gzip',
  'zip',
  'zipPlugin',
  'tar',
  'tarPlugin',
  'getscript',
  'getscriptPlugin',
  'vimball',
  'vimballPlugin',
  '2html_plugin',
  'logipat',
  'rrhelper',
  'spellfile_plugin',
  'matchit',
}

local xdg_data = os.getenv 'XDG_DATA_HOME' or os.getenv 'HOME' .. '/.local/share'
local xdg_config = os.getenv 'XDG_CONFIG_HOME' or os.getenv 'HOME' .. '/.config'
local xdg_cache = os.getenv 'XDG_CACHE_HOME' or os.getenv 'HOME' .. '/.cache'
local nvim_name = '/nvim-vscode'

vim.fn.stdpath = function(val)
  if val == 'data' then
    return xdg_data .. nvim_name
  elseif val == 'config' then
    return xdg_config .. nvim_name
  elseif val == 'cache' then
    return xdg_cache .. nvim_name
  elseif val == 'log' or val == 'state' then
    return os.getenv 'HOME' .. '/.local/state' .. nvim_name
  else
    return old_path(val)
  end
end
o.runtimepath =
  [[/etc/xdg/nvim,~/.local/share/flatpak/exports/share/nvim/site,/var/lib/flatpak/exports/share/nvim/site,/usr/local/share/nvim/site,/usr/share/nvim/site,/usr/share/nvim/runtime,/usr/lib/nvim,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/var/lib/flatpak/exports/share/nvim/site/after,~/.local/share/flatpak/exports/share/nvim/site/after,/etc/xdg/nvim/after,/usr/share/vim/vimfiles]]
o.runtimepath:prepend(vim.fn.stdpath 'config')
o.runtimepath:append({
  vim.fn.stdpath 'data' .. '/site',
  vim.fn.stdpath 'data' .. '/site/after',
  vim.fn.stdpath 'config' .. '/after',
})
o.packpath =
  [[/etc/xdg/nvim,~/.local/share/flatpak/exports/share/nvim/site,/var/lib/flatpak/exports/share/nvim/site,/usr/local/share/nvim/site,/usr/share/nvim/site,/usr/share/nvim/runtime,/usr/lib/nvim,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/var/lib/flatpak/exports/share/nvim/site/after,~/.local/share/flatpak/exports/share/nvim/site/after,/etc/xdg/nvim/after]]
o.packpath:prepend({
  vim.fn.stdpath 'config' .. '/after',
  vim.fn.stdpath 'config',
  vim.fn.stdpath 'data' .. '/site/after',
  vim.fn.stdpath 'data' .. '/site',
})

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('options')
require('lazy').setup('plugins',{
  performance =  {
    rtp = {
      disabled_plugins = disabled_built_ins
    }
  }
})
-- map('n', 'j', 'gj', { noremap = false })
-- map('n', 'k', 'gk', { noremap = false })
-- map('i', ',', ',<C-g>u')
-- map('i', '.', '.<C-g>u')
-- map('i', '?', '?<C-g>u')

-- map('n', '<M-->', ':exe "vertical resize -10"<CR>')
-- map('n', '<M-=>', ':exe "vertical resize +10"<CR>')

-- map('x', '<leader>y', '"+y')
-- map('x', '<leader>p', '"+p')
--
-- Plugin mappings
-- map('n', '<F7>', ':ColorizerToggle<cr>')
--
--
-- if vim.g.vscode then
-- 	-- cmd [[ nmap j gj ]]
-- 	-- cmd [[ nmap k gk ]]
--
-- 	map('n', '<C-Up>', "<Cmd>call VSCodeNotify('editor.action.insertCursorAbove')<cr>", { noremap = false })
-- 	map('n', '<C-Down>', "<Cmd>call VSCodeNotify('editor.action.insertCursorBelow')<cr>", { noremap = false })
-- 	map('n', '<C-Right>', "<Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>")
-- 	map('n', '<C-Left>', "<Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>")
--
-- 	map('n', '<leader>gg', "<Cmd>call VSCodeNotify('workbench.view.scm')<cr>")
-- 	map('n', '<leader>ge', "<Cmd>call VSCodeNotify('workbench.view.extensions')<cr>")
-- 	map('n', '<leader>gs', "<Cmd>call VSCodeNotify('workbench.view.search.toggleVisibility')<cr>")
-- 	map('n', '<leader>gf', "<Cmd>call VSCodeNotify('workbench.view.explorer')<cr>")
-- 	map('n', '<leader>gk', "<Cmd>call VSCodeNotify('workbench.action.openGlobalKeybindings')<cr>")
