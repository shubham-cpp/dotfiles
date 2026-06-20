if vim.g.current_compiler then return end
vim.g.current_compiler = "odin"

vim.bo.makeprg = "odin check . -vet -terse-errors"
-- vim.bo.errorformat = "%f(%l:%c) %m"
vim.bo.errorformat = "%f(%l:%c) %t%*\\a: %m"
