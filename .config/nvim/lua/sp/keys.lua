local map = require('sp.util').map

map('', '0', '^', { noremap = false, silent = true })
map('n', ',w', '<cmd>w!<cr>', { desc = 'Save File' })
map('n', ',W', '<cmd>noautocmd w!<cr>', { desc = 'Save File(without autocmds)' })
map('n', '<Esc>', ':nohl<cr>')
map('n', '*', '*zz')
map('n', '#', '#zz')
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')

map('n', '<M-j>', 'mz:m+<cr>`z')
map('n', '<M-k>', 'mz:m-2<cr>`z')
map('n', '<S-Left>', ':tabp<cr>')
map('n', '<S-Right>', ':tabn<cr>')

map('n', '<C-h>', '<C-W>h')
map('n', '<C-j>', '<C-W>j')
map('n', '<C-k>', '<C-W>k')
map('n', '<C-l>', '<C-W>l')

vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Pasting in visual mode doesn't copy
vim.keymap.set('x', 'p', [[ 'pgv"'.v:register.'y' ]], { expr = true })
map('n', 'dl', '"_dl')
map('n', 'c', '"_c')
map('n', 'C', '"_C')
map('x', 'c', '"_c')

map('c', '<C-a>', '<Home>', { silent = false })
map('c', '<C-e>', '<End>', { silent = false })

vim.cmd [[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]]
map('x', ',s', '"sy:let @/=@s<CR>cgn')

map('v', 'J', ":m '>+1<CR>gv=gv")
map('v', 'K', ":m '<-2<CR>gv=gv")
map('x', '<', '<gv')
map('x', '>', '>gv')

map('i', ',', ',<C-g>u')
map('i', '.', '.<C-g>u')
map('i', '?', '?<C-g>u')

map('x', '<leader>y', '"+y')
map('x', '<leader>p', '"+p')

map('o', 'ie', ':exec "normal! ggVG"<cr>')
map('o', 'iv', ':exec "normal! HVL"<cr>')
