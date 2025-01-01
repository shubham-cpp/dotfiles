vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'
vim.g.base46_cache = vim.fn.stdpath 'data' .. '/base46_cache/'

vim.t['bufs'] = vim.t.bufs and vim.t.bufs or vim.api.nvim_list_bufs() -- initialize buffers for the current tab

require 'lazy_init'
