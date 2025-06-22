vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  { import = "plugins" },
  { import = "plugins.lsp" },
}, lazy_config)

require 'configs.options'
require 'configs.autocmds'
require 'configs.keymaps'

vim.cmd.colorscheme 'vague'
