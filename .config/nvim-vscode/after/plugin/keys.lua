local map = function(mode, lhs, rhs, options)
  local opts = (options == nil or next(options) == nil) and { silent = true, noremap = true } or options
  vim.keymap.set(mode, lhs, rhs, opts)
end

map('', '0', '^', { noremap = false, silent = true })
map('n', '<Esc>', ':nohl<cr>')
map('n', '*', '*zz')
map('n', '#', '#zz')
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')
map('n', '_', 'ggY``P')

map('n', '<M-j>', 'mz:m+<cr>`z')
map('n', '<M-k>', 'mz:m-2<cr>`z')

map('x', 'p', [[ 'pgv"'.v:register.'y' ]], { noremap = true, expr = true })
map('n', 'dl', '"_dl')
map('n', 'c', '"_c')
map('n', 'C', '"_C')
map('x', 'c', '"_c')

map('x', 'J', ":m '>+1<CR>gv=gv")
map('x', 'K', ":m '<-2<CR>gv=gv")
map('x', '<', '<gv')
map('x', '>', '>gv')
map('o', 'ie', ':exec "normal! ggVG"<cr>', { noremap = true, silent = true })
map('o', 'iv', ':exec "normal! HVL"<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count ? 'j' : 'gj'", { noremap = true, expr = true })
vim.api.nvim_set_keymap('n', 'k', "v:count ? 'k' : 'gk'", { noremap = true, expr = true })

if vim.g.vscode then
  map('n', 'j', "<Cmd>call VSCodeNotify('cursorDown')<cr>")
  map('n', 'k', "<Cmd>call VSCodeNotify('cursorUp')<cr>")
  map('n', ',w', "<Cmd>call VSCodeNotify('workbench.action.files.save')<cr>")
  map('n', ',W', "<Cmd>call VSCodeNotify('workbench.action.files.saveWithoutFormatting')<cr>")
  map('n', 'gh', "<Cmd>call VSCodeNotify('editor.action.showHover')<cr>")
  map('', 'gD', "<Cmd>call VSCodeNotify('editor.action.revealDefinitionAside')<CR>")
  map('', 'gr', "<Cmd>call VSCodeNotify('editor.action.goToReferences')<CR>")
  map('', '\\s', "<Cmd>call VSCodeNotify('editor.action.toggleStickyScroll')<CR>")

  map('n', '<leader>=', "<Cmd>call VSCodeNotify('editor.action.formatDocument')<cr>")
  map('n', ']q', "<Cmd>call VSCodeNotify('search.action.focusNextSearchResult')<cr>")
  map('n', '[q', "<Cmd>call VSCodeNotify('search.action.focusPreviousSearchResult')<cr>")
  map('n', '<leader>j', "<Cmd>call VSCodeNotify('search.action.focusNextSearchResult')<cr>")
  map('n', '<leader>k', "<Cmd>call VSCodeNotify('search.action.focusPreviousSearchResult')<cr>")
  map('n', ']d', "<Cmd>call VSCodeNotify('editor.action.marker.nextInFiles')<cr>")
  map('n', '[d', "<Cmd>call VSCodeNotify('editor.action.marker.prevInFiles')<cr>")
  map(
    'x',
    '<leader>f',
    [[<Cmd>call VSCodeNotifyRangePos('editor.action.formatSelection', line("v"), line("."), col("v"), col("."), 1)<CR>]]
  )
  map('n', '<C-b>', "<Cmd>call VSCodeNotify('workbench.action.toggleSidebarVisibility')<cr>")
  map('n', '<C-j>', "<Cmd>call VSCodeNotify('workbench.action.terminal.toggleTerminal')<cr>")

  map('n', '<leader>o', "<Cmd>call VSCodeNotify('workbench.action.openRecent')<cr>")
  map('', 'za', "<Cmd>call VSCodeNotify('editor.toggleFold')<CR>")
  map('', 'zC', "<Cmd>call VSCodeNotify('editor.foldAll')<CR>")
  map('', 'zO', "<Cmd>call VSCodeNotify('editor.unfoldAll')<CR>")
  map('', 'zp', "<Cmd>call VSCodeNotify('editor.gotoParentFold')<CR>")
else
  map('n', ',w', '<cmd>w!<cr>')
  map('n', ',W', '<Cmd>noautocmd w!<cr>')
  map('n', '<S-Left>', ':tabp<cr>')
  map('n', '<S-Right>', ':tabn<cr>')
  map('n', '<C-Left>', ':tabp<cr>')
  map('n', '<C-Right>', ':tabn<cr>')

  map('n', '<leader>j', '<cmd>cn<cr>')
  map('n', '<leader>k', '<cmd>cprev<cr>')
  map('n', ']q', '<cmd>cn<cr>')
  map('n', '[q', '<cmd>cprev<cr>')
  map('n', ']b', '<cmd>bn<cr>')
  map('n', '[b', '<cmd>bprev<cr>')

  map('n', '<C-h>', '<C-W>h')
  map('n', '<C-j>', '<C-W>j')
  map('n', '<C-k>', '<C-W>k')
  map('n', '<C-l>', '<C-W>l')

  map('c', '<C-a>', '<Home>', { noremap = false, silent = false })
  map('c', '<C-e>', '<End>', { noremap = false, silent = false })

  vim.cmd [[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]]
  map('x', ',s', '"sy:let @/=@s<CR>cgn')
end
