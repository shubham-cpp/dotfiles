vim.loader.enable()
require("vim._core.ui2").enable({ enable = true })

vim.g.mapleader = " "
vim.g.maplocalleader = ","

require("core.options")
require("core.keymaps")
require("core.commands")
require("core.autocmds")
require("plugins.init")
