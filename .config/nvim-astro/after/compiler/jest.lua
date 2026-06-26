if vim.fn.executable "jest" ~= 1 then vim.cmd.CompilerSet [[makeprg=npx\ --no-install\ jest\ --no-colors]] end
