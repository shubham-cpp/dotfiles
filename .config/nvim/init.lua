require("config.options")
require("config.keymaps")
require("config.autocmds")

require("plugins")

vim.cmd.colorscheme("custom-vague")

vim.cmd("packadd cfilter")
vim.cmd("packadd nvim.tohtml")
