return function(client, bufnr)
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
  if client.server_capabilities.inlayHintProvider then
    local inlayhints_avail, inlayhints = pcall(require, 'lsp-inlayhints')
    if inlayhints_avail then
      inlayhints.on_attach(client, bufnr)
      vim.keymap.set('n', '<leader>uh', function()
        inlayhints.toggle()
      end, { desc = 'Toggle inlay hints' })
    end
  end
end
