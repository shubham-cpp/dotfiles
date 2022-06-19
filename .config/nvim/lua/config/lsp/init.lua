-- vim.cmd([[ packadd! cmp-nvim-lsp ]])
-- local utils = require("config.lsp.utils")

require('config.lsp.diagnostics')
require('config.lsp.servers')
-- require("config.lsp.nulls").config(utils.on_attach)
-- vim.cmd(":LspStart null-ls")
