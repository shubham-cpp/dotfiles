local g = vim.g
g.mapleader = ' '

require 'sp.options'
-- require 'sp.keys'

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

require('lazy').setup('sp.plugins', {
  performance = {
    rtp = {
      -- paths = astronvim.supported_configs,
      disabled_plugins = {
        'tohtml',
        'gzip',
        'zipPlugin',
        'netrwPlugin',
        'tarPlugin',
        'vimball',
        'logiPat',
        'rrhelper',
        'matchit',
        'tutor',
      },
    },
  },
  change_detection = {
    -- automatically check for config file changes and reload the ui
    enabled = true,
    notify = false, -- get a notification when changes are found
  },
})
-- require 'sp.autocmds'
