local ok, rt = pcall(require, 'rust-tools')
local bmap = function(lhs, rhs, buf)
  vim.keymap.set('n', lhs, rhs, { noremap = true, silent = false, buffer = buf })
end
if not ok then
  return
end

local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()
cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = { 'documentation', 'detail', 'additionalTextEdits' },
}
local goto_references = function()
  local ok_fzf, fzf = pcall(require, 'fzf-lua')
  if ok_fzf then
    fzf.lsp_references({
      winopts = {
        preview = {
          layout = 'vertical',
          vertical = 'up:40%',
        },
      },
    })
  else
    vim.lsp.buf.references()
  end
end
rt.setup({
  server = {
    on_attach = function(_, bufnr)
      bmap('K', rt.hover_actions.hover_actions, bufnr)
      bmap('gac', vim.lsp.buf.code_action, bufnr)
      bmap('J', rt.join_lines.join_lines, bufnr)
      bmap('gd', vim.lsp.buf.definition, bufnr)
      bmap('gr', goto_references, bufnr)
      bmap('gR', rt.workspace_refresh.reload_workspace, bufnr)
      bmap('gw', vim.lsp.buf.document_symbol, bufnr)
      bmap('gW', vim.lsp.buf.workspace_symbol, bufnr)
      bmap('<F2>', vim.lsp.buf.rename, bufnr)
      bmap('gl', vim.diagnostic.open_float, bufnr)
      bmap('[g', vim.diagnostic.goto_prev, bufnr)
      bmap(']g', vim.diagnostic.goto_next, bufnr)
      print 'Rust Lsp has Started'
    end,
    capabilities = cmp_capabilities,
    flags = { debounce_text_changes = 150 },
  },
})
