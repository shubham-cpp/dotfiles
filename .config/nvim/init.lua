local o = vim.opt
local g = vim.g
local old_path = vim.fn.stdpath
g.mapleader = ' '

local xdg_data = os.getenv 'XDG_DATA_HOME' or os.getenv 'HOME' .. '/.local/share'
local xdg_config = os.getenv 'XDG_CONFIG_HOME' or os.getenv 'HOME' .. '/.config'
local xdg_cache = os.getenv 'XDG_CACHE_HOME' or os.getenv 'HOME' .. '/.cache'

vim.fn.stdpath = function(val)
  if val == 'data' then
    return xdg_data .. '/nvim-temp'
  elseif val == 'config' then
    return xdg_config .. '/nvim-temp'
  elseif val == 'cache' then
    return xdg_cache .. '/nvim-temp'
  else
    return old_path(val)
  end
end
o.runtimepath =
  [[/etc/xdg/nvim,~/.local/share/flatpak/exports/share/nvim/site,/var/lib/flatpak/exports/share/nvim/site,/usr/local/share/nvim/site,/usr/share/nvim/site,/usr/share/nvim/runtime,/usr/lib/nvim,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/var/lib/flatpak/exports/share/nvim/site/after,~/.local/share/flatpak/exports/share/nvim/site/after,/etc/xdg/nvim/after,/usr/share/vim/vimfiles]]
o.runtimepath:prepend '~/.config/nvim-temp'
o.runtimepath:append({
  '~/.local/share/nvim-temp/site',
  '~/.local/share/nvim-temp/site/after',
  '~/.config/nvim-temp/after',
})
o.packpath =
  [[/etc/xdg/nvim,~/.local/share/flatpak/exports/share/nvim/site,/var/lib/flatpak/exports/share/nvim/site,/usr/local/share/nvim/site,/usr/share/nvim/site,/usr/share/nvim/runtime,/usr/lib/nvim,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/var/lib/flatpak/exports/share/nvim/site/after,~/.local/share/flatpak/exports/share/nvim/site/after,/etc/xdg/nvim/after]]
o.packpath:prepend({
  vim.fn.stdpath 'config' .. '/after',
  vim.fn.stdpath 'config',
  vim.fn.stdpath 'data' .. '/site/after',
  vim.fn.stdpath 'data' .. '/site',
})

require 'sp.options'
require 'sp.keys'

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

require('lazy').setup 'sp.plugins'
require 'sp.autocmds'
