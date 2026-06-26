if vim.fn.executable "eslint_d" == 1 then
  vim.cmd.CompilerSet [[makeprg=eslint_d\ --fix\ --format=stylish]]
else
  vim.cmd.CompilerSet [[makeprg=npx\ eslint\ --fix\ --format=stylish]]
end
