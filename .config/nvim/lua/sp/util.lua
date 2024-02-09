local M = {}

M.bun_path = function()
  local bun = os.getenv 'BUN_INSTALL'
  local xdg_data = os.getenv 'XDG_DATA_HOME' or os.getenv 'HOME' .. '/.local/share'
  if not bun then
    return xdg_data .. '/bun/bin'
  end
  return bun .. '/bin'
end

M.current_buf, M.last_buf = nil, nil

--- A wrapper around `vim.keymap.set`
---@param mode string|table 'n'|'v'|'o'
---@param lhs string
---@param rhs string|function
---@param opts table<string,boolean | string>? (default is `{noremap = true, silent = true}`)
M.map = function(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts and next(opts) ~= nil then
    vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

-- Picked up from Astronvim

--- Close a given buffer
---@param bufnr? number The buffer to close or the current buffer if not provided
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close(bufnr, force)
  if not bufnr or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end

  local buftype = vim.api.nvim_get_option_value('buftype', { buf = bufnr })
  vim.cmd(('silent! %s %d'):format((force or buftype == 'terminal') and 'bdelete!' or 'confirm bdelete', bufnr))
end

--- Close all buffers
---@param keep_current? boolean Whether or not to keep the current buffer (default: false)
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close_all(keep_current, force)
  if keep_current == nil then
    keep_current = false
  end
  local current = vim.api.nvim_get_current_buf()
  for _, bufnr in ipairs(vim.t.bufs) do
    if not keep_current or bufnr ~= current then
      M.close(bufnr, force)
    end
  end
end

--- Close buffers to the left of the current buffer
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close_left(force)
  local current = vim.api.nvim_get_current_buf()
  for _, bufnr in ipairs(vim.t.bufs) do
    if bufnr == current then
      break
    end
    M.close(bufnr, force)
  end
end

--- Close buffers to the right of the current buffer
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close_right(force)
  local current = vim.api.nvim_get_current_buf()
  local after_current = false
  for _, bufnr in ipairs(vim.t.bufs) do
    if after_current then
      M.close(bufnr, force)
    end
    if bufnr == current then
      after_current = true
    end
  end
end

--- Check if a buffer is valid
---@param bufnr number The buffer to check
---@return boolean # Whether the buffer is valid or not
function M.is_valid(bufnr)
  if not bufnr then
    bufnr = 0
  end
  return vim.api.nvim_buf_is_valid(bufnr) and vim.bo[bufnr].buflisted
end

M.symbols = {
  cmp_kinds = {
    Text = '  ',
    Method = '  ',
    Function = '  ',
    Field = '  ',
    Variable = '  ',
    Interface = '  ',
    Module = '  ',
    Property = '  ',
    Value = '  ',
    Enum = '  ',
    Keyword = '  ',
    Color = '  ',
    File = '  ',
    Folder = '  ',
    EnumMember = '  ',
    Constant = '  ',
    Struct = '  ',
    Event = '  ',
    Operator = '  ',
    TypeParameter = '  ',
    Array = '󰅪',
    Boolean = '⊨',
    Class = '󰌗',
    Constructor = '',
    Key = '󰌆',
    Namespace = '󰅪',
    Null = 'NULL',
    Number = '#',
    Object = '󰀚',
    Package = '󰏗',
    Reference = '',
    Snippet = '',
    String = '󰀬',
    Unit = '',
  },
  lsp_kinds = {
    mode = 'symbol',
    symbol_map = {
      Array = '󰅪',
      Boolean = '⊨',
      Class = '󰌗',
      Constructor = '',
      Key = '󰌆',
      Namespace = '󰅪',
      Null = 'NULL',
      Number = '#',
      Object = '󰀚',
      Package = '󰏗',
      Property = '',
      Reference = '',
      Snippet = '',
      String = '󰀬',
      TypeParameter = '󰊄',
      Unit = '',
    },
    menu = {},
  },
}

--- Get a hash of the current directory + git branch
---@return string
function M.get_hash()
  local str = 'echo "dir:' .. vim.fn.getcwd()
  if vim.b.gitsigns_head then
    str = str .. ';git:' .. vim.b.gitsigns_head
  end
  -- vim.print(str)
  local hash = vim.fn.system(str .. "\" | md5sum | awk '{print $1}'")
  --[[ Without awk
  local hash = vim.fn.system(str .. "\" | md5sum")
  local first_space_index = string.find(hash, " ")
  if first_space_index then
    return string.sub(hash, 1, first_space_index - 1)
  else
    return hash
  end
  --]]
  return hash
end

function M.lsp_organize_imports()
  vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' } }, apply = true })
  vim.lsp.buf.code_action({ context = { only = { 'source.removeUnused' } }, apply = true })
end

M.au_lsp = vim.api.nvim_create_augroup('sp_lsp', { clear = true })

function M.on_attach(client, bufnr)
  local ok_fzf, _ = pcall(require, 'fzf-lua')
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
  vim.keymap.set('i', '<C-h>', vim.lsp.buf.signature_help, { buffer = bufnr })
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { buffer = bufnr, desc = 'Goto Implementation' })
  vim.keymap.set('n', 'gs', M.lsp_organize_imports, { buffer = bufnr, desc = 'Organize Imports' })

  if not ok_fzf then
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr, desc = 'Goto Definition' })
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = bufnr, desc = 'Goto Declaration' })
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = bufnr, desc = 'Lsp References' })
    vim.keymap.set('n', 'gw', vim.lsp.buf.document_symbol, { buffer = bufnr, desc = 'Document Symbols' })
    vim.keymap.set('n', 'gW', vim.lsp.buf.workspace_symbol, { buffer = bufnr, desc = 'Workspace Symbols' })
    vim.keymap.set('n', 'gt', vim.lsp.buf.type_definition, { buffer = bufnr, desc = 'Type Definition' })
  end

  vim.keymap.set('n', 'gac', vim.lsp.buf.code_action, { buffer = bufnr, desc = 'Code Actions' })
  vim.keymap.set('n', '<F2>', vim.lsp.buf.rename, { buffer = bufnr, desc = 'Lsp Rename' })
  vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, { buffer = bufnr, desc = 'Lsp Code Actions' })
  vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, { buffer = bufnr, desc = 'Lsp Rename' })
  vim.keymap.set('n', 'gl', vim.diagnostic.open_float, { buffer = bufnr, desc = 'diagnostic open' })
  vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { buffer = bufnr, desc = 'Goto Prev Diagnostic' })
  vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { buffer = bufnr, desc = 'Goto Next Diagnostic' })
  vim.keymap.set('n', '[e', function()
    vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR })
  end, { buffer = bufnr, desc = 'Goto Prev Error' })
  vim.keymap.set('n', ']e', function()
    vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR })
  end, { buffer = bufnr, desc = 'Goto Next Error' })

  if client.name == 'prismals' then
    vim.keymap.set('n', '<leader>=', vim.lsp.buf.format, { buffer = bufnr, desc = 'Format Buffer(LSP)' })
  end
  if client.server_capabilities.documentSymbolProvider then
    local ok_navic, navic = pcall(require, 'nvim-navic')
    if ok_navic then
      navic.attach(client, bufnr)
    end
  end
  if client.server_capabilities.definitionProvider == true then
    vim.api.nvim_buf_set_option(bufnr, 'tagfunc', 'v:lua.vim.lsp.tagfunc')
  end

  if client.server_capabilities.documentFormattingProvider == true then
    vim.api.nvim_buf_set_option(bufnr, 'formatexpr', 'v:lua.vim.lsp.formatexpr()')
    -- Add this <leader> bound mapping so formatting the entire document is easier.
    -- M.map('n', '<leader>gq', '<cmd>lua vim.lsp.buf.formatting()<CR>', {desc="Format Expr"})
  end

  if client.name == 'svelte' then
    vim.api.nvim_create_autocmd('BufWritePost', {
      pattern = { '*.js', '*.ts' },
      group = M.au_lsp,
      callback = function(ctx)
        client.notify('$/onDidChangeTsOrJsFile', { uri = ctx.file })
      end,
    })
  end
  if client.server_capabilities.documentHighlightProvider then
    vim.api.nvim_exec(
      [[
		        hi LspReferenceWrite cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
		        hi LspReferenceRead cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
		        hi LspReferenceText cterm=bold ctermfg=red gui=bold guisp= guifg=#7bcbfa guibg=#565575
		        hi LspDiagnosticsDefaultError cterm=bold ctermbg=red guifg=#ff3333
		        hi LspDiagnosticsDefaultWarning cterm=bold ctermbg=red guifg=#e7ae05
		        augroup lsp_document_highlight
		        autocmd! * <buffer>
		        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
		        " autocmd CursorHold <buffer> lua vim.diagnostic.open_float(0, {scope='line'})
		        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
		        " autocmd CursorHoldI <buffer> silent! lua vim.lsp.buf.signature_help()
		        augroup END
		    ]],
      false
    )
  end
end

return M
