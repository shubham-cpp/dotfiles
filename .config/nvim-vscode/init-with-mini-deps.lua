-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath "data" .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
  vim.cmd 'echo "Installing `mini.nvim`" | redraw'
  local clone_cmd = { "git", "clone", "--filter=blob:none", "https://github.com/echasnovski/mini.nvim", mini_path }
  vim.fn.system(clone_cmd)
  vim.cmd "packadd mini.nvim | helptags ALL"
  vim.cmd 'echo "Installed `mini.nvim`" | redraw'
end

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Set up 'mini.deps' (customize to your liking)
require("mini.deps").setup({ path = { package = path_package } })

local walk_files = require("u.utils").walk_files

walk_files(vim.fn.stdpath "config" .. "/lua/u/plugins/mini", function(path, type)
  if type == "directory" or type == "dir" or path:match "^no%-vscode" then
    goto continue
  end

  local name_without_ext = path:match "(.+)%.%w+$" or path
  require("u.plugins.mini." .. name_without_ext)

  ::continue::
end)

walk_files(vim.fn.stdpath "config" .. "/lua/u/plugins", function(path, type)
  if type == "directory" or type == "dir" or path:match "^no%-vscode" then
    goto continue
  end

  local name_without_ext = path:match "(.+)%.%w+$" or path
  require("u.plugins." .. name_without_ext)

  ::continue::
end)

if vim.g.vscode == nil then
  walk_files(vim.fn.stdpath "config" .. "/lua/u/plugins/no-vscode", function(path, type)
    if type == "directory" or type == "dir" or path:match "^no%-vscode" then
      goto continue
    end

    local name_without_ext = path:match "(.+)%.%w+$" or path
    require("u.plugins.no-vscode." .. name_without_ext)

    ::continue::
  end)
end

require "u.config.options"
require "u.config.keymaps"
require "u.config.autocmds"
