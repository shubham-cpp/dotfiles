return function(client)
  if require('lspconfig').util.root_pattern('deno.json', 'deno.jsonc')(vim.fn.getcwd()) then
    if client.name == 'tsserver' or client.name == 'eslint' then
      client.stop()
      return
    end
  elseif require('lspconfig').util.root_pattern('packages.json', 'tsconfig.json')(vim.fn.getcwd()) then
    if client.name == 'denols' then
      client.stop()
      return
    end
  end
end
